function isEditorField(el) {
  return el instanceof HTMLTextAreaElement && el.classList.contains("note-editor-field");
}

function isImageCapableField(el) {
  return isEditorField(el) && el.closest(".review-box") != null;
}

function focusField(el) {
  el.focus();
  el.setSelectionRange(el.value.length, el.value.length);
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
      resolve(dataUrl.slice(dataUrl.indexOf(",") + 1));
    };
    img.onerror = () => {
      URL.revokeObjectURL(objectUrl);
      reject(new Error("Could not load image"));
    };
    img.src = objectUrl;
  });
}

async function dispatchImageData(el, file) {
  try {
    const base64 = await blobToPngBase64(file);
    el.dispatchEvent(new CustomEvent("typst-image-data", { detail: { data: base64 }, bubbles: true }));
  } catch (err) {
    el.dispatchEvent(
      new CustomEvent("typst-image-error", {
        detail: { message: "Could not read that image. Try a different file." },
        bubbles: true,
      })
    );
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
    await dispatchImageData(el, file);
  };
  input.click();
}

function setupImageInsertion() {
  document.addEventListener(
    "paste",
    async (event) => {
      const el = event.target;
      if (!isImageCapableField(el)) return;
      const file = imageFileFromClipboard(event.clipboardData);
      if (!file) return;
      event.preventDefault();
      await dispatchImageData(el, file);
    },
    true
  );

  document.addEventListener("dragover", (event) => {
    event.preventDefault();
  });

  document.addEventListener("drop", async (event) => {
    event.preventDefault();
    const el = event.target;
    if (!isImageCapableField(el)) return;
    const file = imageFileFromDataTransfer(event.dataTransfer);
    if (!file) return;
    focusField(el);
    await dispatchImageData(el, file);
  });

  document.addEventListener("contextmenu", (event) => {
    const el = event.target;
    if (isImageCapableField(el)) {
      event.preventDefault();
      focusField(el);
      openImagePicker(el);
      return;
    }
    if (el instanceof HTMLInputElement || isEditorField(el)) return;
    event.preventDefault();
  });
}

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

function setupBlurFocusFacts() {
  document.addEventListener(
    "blur",
    (event) => {
      const el = event.target;
      if (!isEditorField(el)) return;
      el.dispatchEvent(new CustomEvent("typst-blur", { detail: { windowHasFocus: document.hasFocus() }, bubbles: true }));
    },
    true
  );
}

export function setupEditorNiceties() {
  setupImageInsertion();
  setupResizeHandleTouchSupport();
  setupBlurFocusFacts();
}
