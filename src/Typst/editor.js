const OPEN_TO_CLOSE = {
  "(": ")",
  "[": "]",
  "{": "}",
};

const FENCE_PAIRS = {
  '"': '"',
  "`": "`",
  $: "$",
};

const CLOSE_CHARS = new Set(Object.values(OPEN_TO_CLOSE));

function isEditorField(el) {
  return el instanceof HTMLTextAreaElement && el.classList.contains("note-editor-field");
}

function insertText(el, start, end, text) {
  el.setSelectionRange(start, end);
  document.execCommand("insertText", false, text);
}

function deleteRange(el, start, end) {
  el.setSelectionRange(start, end);
  document.execCommand("delete");
}

function moveCursor(el, position) {
  el.setSelectionRange(position, position);
}

function currentLineIndent(value, pos) {
  const lineStart = value.lastIndexOf("\n", pos - 1) + 1;
  const match = value.slice(lineStart, pos).match(/^[ \t]*/);
  return match ? match[0] : "";
}

function focusField(el) {
  el.focus();
  el.setSelectionRange(el.value.length, el.value.length);
}

const ADD_FIELD_CHAIN = {
  "editable-typst-add-front": "editable-typst-add-back",
};

const MAX_IMAGE_BASE64_LENGTH = 2 * 1024 * 1024;

function blobToPngBase64(blob) {
  return new Promise((resolve, reject) => {
    const objectUrl = URL.createObjectURL(blob);
    const img = new Image();
    img.onload = () => {
      const canvas = document.createElement("canvas");
      canvas.width = img.naturalWidth;
      canvas.height = img.naturalHeight;
      canvas.getContext("2d").drawImage(img, 0, 0);
      URL.revokeObjectURL(objectUrl);
      const dataUrl = canvas.toDataURL("image/png");
      const base64 = dataUrl.slice(dataUrl.indexOf(",") + 1);
      if (base64.length > MAX_IMAGE_BASE64_LENGTH) {
        reject(new Error("too-large"));
        return;
      }
      resolve(base64);
    };
    img.onerror = () => {
      URL.revokeObjectURL(objectUrl);
      reject(new Error("Could not load image"));
    };
    img.src = objectUrl;
  });
}

function insertImageAtCursor(el, base64) {
  const id = crypto.randomUUID();
  const { selectionStart: start, selectionEnd: end } = el;
  const reference = `#image("${id}.png", width: 100%)`;
  insertText(el, start, end, reference);
  el.dispatchEvent(new CustomEvent("tide-image-added", { detail: { id, data: base64 }, bubbles: true }));
}

async function convertAndInsertImage(el, file) {
  try {
    const base64 = await blobToPngBase64(file);
    insertImageAtCursor(el, base64);
  } catch (err) {
    if (err instanceof Error && err.message === "too-large") {
      alert("That image is too large to add to a card (limit ~1.5MB). Try a smaller image or a screenshot of just the relevant part.");
    }
  }
}

function imageFileFromClipboard(clipboardData) {
  if (!clipboardData) return null;
  for (const item of clipboardData.items) {
    if (item.kind === "file" && item.type.startsWith("image/")) {
      return item.getAsFile();
    }
  }
  return null;
}

function imageFileFromDataTransfer(dataTransfer) {
  if (!dataTransfer) return null;
  for (const file of dataTransfer.files) {
    if (file.type.startsWith("image/")) return file;
  }
  return null;
}

function openImagePicker(el) {
  const input = document.createElement("input");
  input.type = "file";
  input.accept = "image/*";
  input.onchange = async () => {
    const file = input.files[0];
    if (!file) return;
    await convertAndInsertImage(el, file);
  };
  input.click();
}

function setupImageInsertion() {
  document.addEventListener(
    "paste",
    async (event) => {
      const el = event.target;
      if (!isEditorField(el)) return;
      const file = imageFileFromClipboard(event.clipboardData);
      if (!file) return;
      event.preventDefault();
      await convertAndInsertImage(el, file);
    },
    true
  );

  document.addEventListener("dragover", (event) => {
    event.preventDefault();
  });

  document.addEventListener("drop", async (event) => {
    event.preventDefault();
    const el = event.target;
    if (!isEditorField(el)) return;
    const file = imageFileFromDataTransfer(event.dataTransfer);
    if (!file) return;
    focusField(el);
    await convertAndInsertImage(el, file);
  });

  document.addEventListener("contextmenu", (event) => {
    const el = event.target;
    if (isEditorField(el)) {
      event.preventDefault();
      focusField(el);
      openImagePicker(el);
      return;
    }
    if (el instanceof HTMLInputElement) return;
    event.preventDefault();
  });
}

// The field-resize handle's drag logic lives entirely in Elm
// (EditableTypst's HandlePressed/HandleDragged/HandleReleased, driven by
// Browser.Events.onMouseMove/onMouseUp) and iOS never fires mouse events
// from a touch — so dragging the handle silently did nothing there.
// Forwarding touch events to synthetic MouseEvents lets that existing
// mouse-only logic work unchanged on touch devices too.
function setupResizeHandleTouchSupport() {
  let dragging = false;

  function forwardAsMouseEvent(type, touch, target) {
    target.dispatchEvent(
      new MouseEvent(type, {
        bubbles: true,
        cancelable: true,
        clientX: touch.clientX,
        clientY: touch.clientY,
      })
    );
  }

  document.addEventListener(
    "touchstart",
    (event) => {
      const handle = event.target.closest(".note-editor-resize-handle");
      if (!handle) return;
      dragging = true;
      event.preventDefault();
      forwardAsMouseEvent("mousedown", event.touches[0], handle);
    },
    { passive: false }
  );

  document.addEventListener(
    "touchmove",
    (event) => {
      if (!dragging) return;
      event.preventDefault();
      forwardAsMouseEvent("mousemove", event.touches[0], document);
    },
    { passive: false }
  );

  const endDrag = (event) => {
    if (!dragging) return;
    dragging = false;
    forwardAsMouseEvent("mouseup", event.changedTouches[0], document);
  };

  document.addEventListener("touchend", endDrag);
  document.addEventListener("touchcancel", endDrag);
}

// Sentinel shared between setupTapOutsideBlur and setupWindowBlurGuard: a
// blur we deliberately triggered (tap outside a field) should always go
// through — only a blur we *didn't* ask for (the OS taking focus away from
// the whole window/app) should ever be suppressed.
let intentionalBlur = false;

// On desktop, clicking anywhere outside a focused textarea naturally blurs
// it, which is what commits the draft and collapses the field back to its
// preview. WKWebView on iOS doesn't reliably do this for taps on plain,
// non-interactive elements (a well-known mobile-web quirk) — the field
// just stays open until something explicitly focusable steals focus. This
// forces the same "tap outside commits" behavior everywhere.
//
// This deliberately finds the open field via Elm's own visible state (the
// `review-box--editing` class) rather than tracking native focus/blur
// events: confirmed by direct testing, re-focusing a field on iOS shortly
// after it was last blurred (e.g. tapping the collapsed preview to start
// editing it a second time) can silently fail to produce a real native
// `focus` event at all, even though Elm's model correctly flips into
// editing and the textarea is visibly shown — so anything keyed off "which
// field last received a real focus event" goes stale after the first
// close/reopen cycle. The class is driven straight from Elm's model, so it
// can't desync like that.
//
// The safe zone is the whole `.review-box`, not just the field/handle —
// that div has its own `onClick EditStarted` covering its entire area
// (that's how tapping the collapsed preview opens editing in the first
// place). A tap on, say, the draft-preview strip below the textarea is
// still "inside" this same box: if the safe zone were narrower, we'd blur
// the field here, but the tap's synthesized `click` event fires right
// after and bubbles up to that same review-box's `onClick`, which by then
// sees `isEditing` already flipped back to false and immediately reopens
// it — the field visibly flashes shut and pops back open. Scoping the
// safe zone to the whole box means a tap can only ever blur a *different*
// field's box, never its own.
function setupTapOutsideBlur() {
  const handler = (event) => {
    const openBox = document.querySelector(".review-box--editing");
    if (!openBox || openBox.contains(event.target)) return;
    const field = openBox.querySelector(".note-editor-field");
    if (!field) return;
    intentionalBlur = true;
    // Real .blur() first, in case the field genuinely does hold native
    // focus (dismisses the on-screen keyboard); a synthetic `blur` event
    // is dispatched unconditionally right after so Elm's onBlur/Committed
    // fires even when the browser's own focus tracking silently didn't
    // follow along — see the comment above.
    field.blur();
    field.dispatchEvent(new FocusEvent("blur"));
    // Deliberately not reset here: WKWebView appears to dispatch the real
    // `blur` event asynchronously (presumably bridged through the native
    // keyboard-dismiss animation) rather than synchronously within this
    // call, unlike desktop browsers. Resetting the flag right after
    // calling .blur() meant it was already back to false by the time
    // setupWindowBlurGuard's listener actually ran, so it never recognized
    // this as an intentional blur — it's the guard's own listener below
    // that consumes and clears the flag once it's actually had a chance to
    // see it.
  };
  document.addEventListener("pointerdown", handler, true);
  document.addEventListener("touchstart", handler, true);
}

function setupWindowBlurGuard() {
  let fieldToRefocus = null;

  document.addEventListener(
    "blur",
    (event) => {
      const el = event.target;
      if (!isEditorField(el)) return;
      if (intentionalBlur) {
        intentionalBlur = false;
        return;
      }
      if (!document.hasFocus()) {
        event.stopImmediatePropagation();
        fieldToRefocus = el;
      }
    },
    true
  );

  window.addEventListener("focus", () => {
    if (fieldToRefocus) {
      const el = fieldToRefocus;
      fieldToRefocus = null;
      el.focus();
    }
  });
}

export function setupEditorNiceties() {
  setupImageInsertion();
  setupWindowBlurGuard();
  setupResizeHandleTouchSupport();
  setupTapOutsideBlur();
  document.addEventListener(
    "keydown",
    (event) => {
      const el = event.target;
      if (!isEditorField(el)) return;

      const { value, selectionStart: start, selectionEnd: end } = el;
      const key = event.key;

      if (key === "Escape") {
        event.preventDefault();
        event.stopPropagation();
        el.blur();
        return;
      }

      if (key === "Enter" && event.shiftKey) {
        const nextId = ADD_FIELD_CHAIN[el.id];
        if (nextId) {
          event.preventDefault();
          const nextEl = document.getElementById(nextId);
          if (nextEl) focusField(nextEl);
          return;
        }
        if (el.id === "editable-typst-add-back") {
          event.preventDefault();
          const submitButton = document.getElementById("add-submit-button");
          if (submitButton) submitButton.click();
          return;
        }
      }

      if (OPEN_TO_CLOSE[key] || FENCE_PAIRS[key]) {
        const close = OPEN_TO_CLOSE[key] || FENCE_PAIRS[key];

        if (start !== end) {
          event.preventDefault();
          const selected = value.slice(start, end);
          insertText(el, start, end, key + selected + close);
          el.setSelectionRange(start + 1, start + 1 + selected.length);
          return;
        }

        if (FENCE_PAIRS[key] && value[start] === key) {
          event.preventDefault();
          moveCursor(el, start + 1);
          return;
        }

        event.preventDefault();
        insertText(el, start, start, key + close);
        moveCursor(el, start + 1);
        return;
      }

      if (CLOSE_CHARS.has(key) && start === end && value[start] === key) {
        event.preventDefault();
        moveCursor(el, start + 1);
        return;
      }

      if (key === "Backspace" && start === end && start > 0) {
        const before = value[start - 1];
        const after = value[start];
        const isPair = (OPEN_TO_CLOSE[before] && OPEN_TO_CLOSE[before] === after) || (FENCE_PAIRS[before] && FENCE_PAIRS[before] === after);
        if (isPair) {
          event.preventDefault();
          deleteRange(el, start - 1, start + 1);
          return;
        }
      }

      if (key === "Tab") {
        event.preventDefault();
        if (start !== end && value.slice(start, end).includes("\n")) {
          const lineStart = value.lastIndexOf("\n", start - 1) + 1;
          const selected = value.slice(lineStart, end);
          const newSelected = event.shiftKey
            ? selected.replace(/^ {1,2}/gm, "")
            : selected.replace(/^/gm, "  ");
          insertText(el, lineStart, end, newSelected);
          el.setSelectionRange(lineStart, lineStart + newSelected.length);
          return;
        }
        insertText(el, start, end, "  ");
        return;
      }

      if (key === "Enter" && start === end) {
        const indent = currentLineIndent(value, start);
        const before = value[start - 1];
        const after = value[start];

        if ((OPEN_TO_CLOSE[before] && OPEN_TO_CLOSE[before] === after) || (FENCE_PAIRS[before] && FENCE_PAIRS[before] === after)) {
          event.preventDefault();
          const innerIndent = indent + "  ";
          insertText(el, start, end, "\n" + innerIndent + "\n" + indent);
          moveCursor(el, start + 1 + innerIndent.length);
          return;
        }

        if (indent) {
          event.preventDefault();
          insertText(el, start, end, "\n" + indent);
        }
      }
    },
    true
  );
}
