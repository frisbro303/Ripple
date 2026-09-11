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

const MAX_IMAGE_BASE64_LENGTH = 2 * 1024 * 1024;

const MIN_FIELD_HEIGHT = 64;
const MAX_FIELD_HEIGHT = 480;
const DEFAULT_FIELD_HEIGHT = 96;

// Shared by every <typst-code-field> and <typst-note-editor> instance: the
// `focusField` port and Shift+Enter chaining both need to reach a specific
// field by its `field-id`, populated at real-connect time (connectedCallback)
// rather than whenever Elm gets around to patching the DOM, so a lookup can
// never be stale or missing because Elm hasn't rendered yet.
const focusRegistry = new Map();

export function registerFocusable(id, el) {
  if (id) focusRegistry.set(id, el);
}

export function unregisterFocusable(id, el) {
  if (id && focusRegistry.get(id) === el) focusRegistry.delete(id);
}

export function getFocusable(id) {
  return id ? focusRegistry.get(id) : undefined;
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

// Ported from Typst/Highlight.elm's tree->span recursion: `highlight_typst`
// returns { tag, text, children } nodes (a leaf has non-null text).
function renderHighlightNode(node) {
  if (node.text !== null && node.text !== undefined) {
    if (node.tag) {
      const span = document.createElement("span");
      span.className = node.tag;
      span.textContent = node.text;
      return span;
    }
    return document.createTextNode(node.text);
  }
  const span = document.createElement("span");
  if (node.tag) span.className = node.tag;
  for (const child of node.children || []) {
    span.appendChild(renderHighlightNode(child));
  }
  return span;
}

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

// The reusable Typst editing surface: a textarea with a syntax-highlighted
// backdrop, resize handle, and the full keymap (bracket/quote pairing, Tab
// indent, Enter auto-indent, image paste/drop/context-menu insertion). Light
// DOM (no shadow root) — styles.css targets `.note-editor-field` etc.
// directly, and Main.elm's global shortcut handler reads
// `event.target.tagName`, which would be retargeted to this element under a
// shadow root.
export class TypstCodeField extends HTMLElement {
  static get observedAttributes() {
    return ["source", "placeholder", "field-id"];
  }

  constructor() {
    super();
    this._built = false;
    this._lastSelection = null;
    this._pendingRefocus = false;
    this._highlightToken = 0;
    this._onWindowFocus = () => {
      if (this._pendingRefocus) {
        this._pendingRefocus = false;
        this._textarea.focus();
      }
    };
  }

  connectedCallback() {
    if (!this._built) this._build();
    registerFocusable(this.getAttribute("field-id"), this);
    window.addEventListener("focus", this._onWindowFocus);
  }

  disconnectedCallback() {
    unregisterFocusable(this.getAttribute("field-id"), this);
    document.removeEventListener("mousemove", this._onDragMove);
    document.removeEventListener("mouseup", this._onDragUp);
    document.removeEventListener("touchmove", this._onTouchDragMove);
    document.removeEventListener("touchend", this._onTouchDragEnd);
    document.removeEventListener("touchcancel", this._onTouchDragEnd);
    window.removeEventListener("focus", this._onWindowFocus);
  }

  attributeChangedCallback(name, oldVal, newVal) {
    if (!this._built) return;
    if (name === "source") {
      const v = newVal ?? "";
      if (v !== this._textarea.value) this.value = v;
    } else if (name === "placeholder") {
      this._textarea.placeholder = newVal ?? "";
    } else if (name === "field-id") {
      if (oldVal !== newVal) {
        unregisterFocusable(oldVal, this);
        registerFocusable(newVal, this);
        if (newVal) this._textarea.id = newVal;
      }
    }
  }

  get value() {
    return this._textarea.value;
  }

  // The only place `.value` is ever assigned directly (bypassing
  // execCommand) — deliberately, since this path is only used for a genuine
  // external reset (a different card's text), where losing undo history is
  // correct.
  set value(v) {
    if (v === this._textarea.value) return;
    this._textarea.value = v;
    this._refreshHighlight();
  }

  // Synchronous end-to-end: safe to call from within a tap/keydown call
  // stack (no rAF/setTimeout/await anywhere in this chain) so the on-screen
  // keyboard reliably raises on iOS.
  focusField() {
    this._textarea.focus();
    const end = this._textarea.value.length;
    const saved = this._lastSelection;
    if (saved && saved.end <= end) {
      this._textarea.setSelectionRange(saved.start, saved.end);
    } else {
      this._textarea.setSelectionRange(end, end);
    }
  }

  blurField() {
    this._textarea.blur();
  }

  _build() {
    this._built = true;
    const initialValue = this.getAttribute("source") ?? "";

    const wrap = document.createElement("div");
    wrap.className = "note-editor-field-wrap";
    wrap.style.height = DEFAULT_FIELD_HEIGHT + "px";

    const highlight = document.createElement("div");
    highlight.className = "note-editor-highlight";
    const highlightScroll = document.createElement("div");
    highlightScroll.className = "note-editor-highlight-scroll";
    highlightScroll.textContent = initialValue;
    highlight.appendChild(highlightScroll);

    const textarea = document.createElement("textarea");
    textarea.className = "note-editor-field";
    textarea.placeholder = this.getAttribute("placeholder") ?? "";
    textarea.setAttribute("autocorrect", "off");
    textarea.setAttribute("autocapitalize", "off");
    textarea.spellcheck = false;
    textarea.value = initialValue;
    const fieldId = this.getAttribute("field-id");
    if (fieldId) textarea.id = fieldId;

    const handle = document.createElement("div");
    handle.className = "note-editor-resize-handle";

    wrap.append(highlight, textarea, handle);
    this.replaceChildren(wrap);

    this._wrap = wrap;
    this._highlightScroll = highlightScroll;
    this._textarea = textarea;
    this._handle = handle;

    this._setupInput();
    this._setupScrollSync();
    this._setupKeymap();
    this._setupImageInsertion();
    this._setupResize();
    this._setupFocusBlur();

    if (initialValue.trim() !== "") this._refreshHighlight();
  }

  _setupInput() {
    this._textarea.addEventListener("input", () => {
      this._refreshHighlight();
      this.dispatchEvent(new CustomEvent("typst-input", { detail: { value: this.value }, bubbles: true, composed: true }));
    });
  }

  _setupScrollSync() {
    this._textarea.addEventListener("scroll", () => {
      this._highlightScroll.style.transform = `translateY(-${this._textarea.scrollTop}px)`;
    });
  }

  _setupFocusBlur() {
    this._textarea.addEventListener("focus", () => {
      this._pendingRefocus = false;
    });

    // Cmd+Tab away mid-edit: WKWebView/desktop both fire a real `blur` on
    // the textarea when the whole window loses focus, indistinguishable
    // from a normal blur unless we check `document.hasFocus()`. Swallowing
    // it here (not dispatching `typst-blur`) means the wrapping
    // <typst-note-editor> never sees it and never closes/commits; the field
    // is refocused once the window regains focus.
    this._textarea.addEventListener("blur", () => {
      this._lastSelection = { start: this._textarea.selectionStart, end: this._textarea.selectionEnd };
      if (!document.hasFocus()) {
        this._pendingRefocus = true;
        return;
      }
      this.dispatchEvent(new CustomEvent("typst-blur", { detail: { value: this.value }, bubbles: true, composed: true }));
    });
  }

  async _refreshHighlight() {
    const source = this._textarea.value;
    const token = ++this._highlightToken;
    let tree;
    try {
      tree = await window.__TAURI__.core.invoke("highlight_typst", { source });
    } catch (err) {
      return;
    }
    if (token !== this._highlightToken) return;
    this._highlightScroll.replaceChildren(renderHighlightNode(tree));
  }

  _setupKeymap() {
    this._textarea.addEventListener("keydown", (event) => {
      const el = this._textarea;
      const { value, selectionStart: start, selectionEnd: end } = el;
      const key = event.key;

      if (key === "Escape") {
        event.preventDefault();
        this.blurField();
        return;
      }

      if (key === "Enter" && event.shiftKey) {
        const nextId = this.getAttribute("next-field");
        if (nextId) {
          const target = getFocusable(nextId);
          if (target && typeof target.focusField === "function") {
            event.preventDefault();
            target.focusField();
            return;
          }
        }
        const submitSelector = this.getAttribute("submit-selector");
        if (submitSelector) {
          const btn = document.querySelector(submitSelector);
          if (btn) {
            event.preventDefault();
            btn.click();
            return;
          }
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
          const newSelected = event.shiftKey ? selected.replace(/^ {1,2}/gm, "") : selected.replace(/^/gm, "  ");
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
    });
  }

  _insertImageAtCursor(base64) {
    const el = this._textarea;
    const id = crypto.randomUUID();
    const { selectionStart: start, selectionEnd: end } = el;
    const reference = `#image("${id}.png", width: 100%)`;
    insertText(el, start, end, reference);
    this.dispatchEvent(new CustomEvent("typst-image", { detail: { id, data: base64 }, bubbles: true, composed: true }));
  }

  async _convertAndInsertImage(file) {
    try {
      const base64 = await blobToPngBase64(file);
      this._insertImageAtCursor(base64);
    } catch (err) {
      if (err instanceof Error && err.message === "too-large") {
        alert("That image is too large to add to a card (limit ~1.5MB). Try a smaller image or a screenshot of just the relevant part.");
      }
    }
  }

  _openImagePicker() {
    const input = document.createElement("input");
    input.type = "file";
    input.accept = "image/*";
    input.onchange = async () => {
      const file = input.files[0];
      if (!file) return;
      await this._convertAndInsertImage(file);
    };
    input.click();
  }

  _setupImageInsertion() {
    const el = this._textarea;

    el.addEventListener("paste", async (event) => {
      const file = imageFileFromClipboard(event.clipboardData);
      if (!file) return;
      event.preventDefault();
      await this._convertAndInsertImage(file);
    });

    el.addEventListener("dragover", (event) => {
      event.preventDefault();
    });

    el.addEventListener("drop", async (event) => {
      event.preventDefault();
      const file = imageFileFromDataTransfer(event.dataTransfer);
      if (!file) return;
      this.focusField();
      await this._convertAndInsertImage(file);
    });

    el.addEventListener("contextmenu", (event) => {
      event.preventDefault();
      this.focusField();
      this._openImagePicker();
    });
  }

  // Ported verbatim: the drag mechanism itself (Elm-style mousedown/
  // mousemove/mouseup) is unchanged, just internal to the component now.
  // Touch devices never fire mouse events from a touch, so touch input on
  // the handle is forwarded as synthetic MouseEvents to drive the same
  // mouse-only logic.
  _setupResize() {
    const handle = this._handle;
    let drag = null;

    this._onDragMove = (event) => {
      if (!drag) return;
      const h = Math.min(MAX_FIELD_HEIGHT, Math.max(MIN_FIELD_HEIGHT, drag.startHeight + (event.clientY - drag.startY)));
      this._wrap.style.height = h + "px";
    };
    this._onDragUp = () => {
      if (!drag) return;
      drag = null;
      document.removeEventListener("mousemove", this._onDragMove);
      document.removeEventListener("mouseup", this._onDragUp);
    };
    handle.addEventListener("mousedown", (event) => {
      event.preventDefault();
      drag = { startY: event.clientY, startHeight: this._wrap.getBoundingClientRect().height };
      document.addEventListener("mousemove", this._onDragMove);
      document.addEventListener("mouseup", this._onDragUp);
    });

    let touchDragging = false;
    const forwardAsMouseEvent = (type, touch, target) => {
      target.dispatchEvent(
        new MouseEvent(type, {
          bubbles: true,
          cancelable: true,
          clientX: touch.clientX,
          clientY: touch.clientY,
        })
      );
    };

    handle.addEventListener(
      "touchstart",
      (event) => {
        touchDragging = true;
        event.preventDefault();
        forwardAsMouseEvent("mousedown", event.touches[0], handle);
      },
      { passive: false }
    );

    this._onTouchDragMove = (event) => {
      if (!touchDragging) return;
      event.preventDefault();
      forwardAsMouseEvent("mousemove", event.touches[0], document);
    };
    document.addEventListener("touchmove", this._onTouchDragMove, { passive: false });

    this._onTouchDragEnd = (event) => {
      if (!touchDragging) return;
      touchDragging = false;
      forwardAsMouseEvent("mouseup", event.changedTouches[0], document);
    };
    document.addEventListener("touchend", this._onTouchDragEnd);
    document.addEventListener("touchcancel", this._onTouchDragEnd);
  }
}

customElements.define("typst-code-field", TypstCodeField);
