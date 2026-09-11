import { getFocusable, registerFocusable, unregisterFocusable } from "./code-field.js";

function isDarkMode(themeAttr) {
  if (themeAttr === "light") return false;
  if (themeAttr === "dark") return true;
  return window.matchMedia("(prefers-color-scheme: dark)").matches;
}

function inkFor(themeAttr) {
  return isDarkMode(themeAttr) ? "#e8e9ec" : "#1c1e21";
}

// A touch device is held closer and viewed at a higher pixel density than a
// desktop monitor, so the same absolute point size that reads fine on
// desktop feels small on a phone. 14pt mirrors the existing Rust-side
// default; 17pt matches iOS's own standard body text size.
function cardTextSizePt() {
  return window.matchMedia("(pointer: coarse)").matches ? 17 : 14;
}

function referencedImageIds(source) {
  const ids = [];
  const parts = source.split('#image("');
  for (let i = 1; i < parts.length; i++) {
    const first = parts[i].split('"')[0];
    if (first.endsWith(".png")) ids.push(first.slice(0, -4));
  }
  return ids;
}

function imageAttachments(knownImages, pendingImages, source) {
  const all = { ...pendingImages, ...knownImages };
  return referencedImageIds(source)
    .filter((id) => all[id] !== undefined)
    .map((id) => [id + ".png", all[id]]);
}

function svgDataUrl(svg) {
  return "data:image/svg+xml;charset=utf-8," + encodeURIComponent(svg);
}

function sameShallowObject(a, b) {
  const aKeys = Object.keys(a);
  const bKeys = Object.keys(b);
  if (aKeys.length !== bKeys.length) return false;
  return aKeys.every((k) => a[k] === b[k]);
}

// The one currently manually-opened editor (if any) — used by the
// tap-outside-to-close handler below. Deliberately keyed off this
// JS-internal flag rather than native focus tracking: re-focusing a field on
// iOS shortly after it was last blurred (e.g. tapping the collapsed preview
// to start editing it a second time) can silently fail to produce a real
// native `focus` event at all, so anything keyed off "which field last
// received a real focus event" goes stale after the first close/reopen
// cycle.
let openEditor = null;

function closeIfOutside(event) {
  if (!openEditor || openEditor.contains(event.target)) return;
  const editor = openEditor;
  editor.close();
  // Only dismisses the on-screen keyboard — commit already happened
  // synchronously in close() above, so this no longer needs to succeed for
  // the edit to be persisted.
  editor._codeField.blurField();
}

document.addEventListener("pointerdown", closeIfOutside, true);
document.addEventListener("touchstart", closeIfOutside, true);

// The card field (front/back): owns editing-vs-preview visibility, the inner
// <typst-code-field>, both preview layers, and render_typst compiles for
// both the draft (live, while editing) and committed (collapsed preview)
// text. Light DOM, same reasoning as typst-code-field.
export class TypstNoteEditor extends HTMLElement {
  static get observedAttributes() {
    return ["source", "preamble", "placeholder", "shortcut-hint", "theme", "field-id", "next-field", "submit-selector"];
  }

  constructor() {
    super();
    this._built = false;
    this._knownImages = {};
    this._pendingImages = {};
    this._manualEditing = false;
    this._lastCommittedSource = null;
    this._committedResult = null;
    this._draftResult = null;
    this._committedToken = 0;
    this._draftToken = 0;
    this._lastWidth = 0;
  }

  connectedCallback() {
    if (!this._built) this._build();
    registerFocusable(this.getAttribute("field-id"), this);
    if (!this._resizeObserver) {
      this._resizeObserver = new ResizeObserver((entries) => {
        const width = entries[0].contentRect.width;
        // Guard a 0-width read (an ancestor momentarily `display:none`) and
        // skip recompiles for sub-~8px deltas so a scrollbar appearing
        // doesn't thrash the compiler.
        if (width <= 0 || Math.abs(width - this._lastWidth) < 8) return;
        this._lastWidth = width;
        this._recompileAll();
      });
    }
    this._resizeObserver.observe(this);
  }

  disconnectedCallback() {
    unregisterFocusable(this.getAttribute("field-id"), this);
    this._resizeObserver?.disconnect();
    if (openEditor === this) openEditor = null;
    this._committedToken++;
    this._draftToken++;
  }

  attributeChangedCallback(name, oldVal, newVal) {
    if (!this._built) return;
    switch (name) {
      case "source": {
        const v = newVal ?? "";
        if (v !== this._lastCommittedSource) {
          this._lastCommittedSource = v;
          this._codeField.value = v;
          this._manualEditing = false;
          if (openEditor === this) openEditor = null;
          this._syncEditingClass();
          this._recompileAll();
        }
        break;
      }

      case "preamble":
      case "theme":
        this._recompileAll();
        break;

      case "placeholder":
        this._codeField.setAttribute("placeholder", newVal ?? "");
        break;

      case "shortcut-hint":
        this._hintEl.textContent = newVal ?? "";
        break;

      case "next-field":
      case "submit-selector":
        if (newVal == null) this._codeField.removeAttribute(name);
        else this._codeField.setAttribute(name, newVal);
        break;

      case "field-id":
        if (oldVal !== newVal) {
          unregisterFocusable(oldVal, this);
          registerFocusable(newVal, this);
        }
        break;
    }
  }

  get value() {
    return this._codeField ? this._codeField.value : this._lastCommittedSource ?? "";
  }

  get knownImages() {
    return this._knownImages;
  }

  set knownImages(images) {
    const next = images || {};
    if (sameShallowObject(next, this._knownImages)) {
      this._knownImages = next;
      return;
    }
    this._knownImages = next;
    if (this._built) this._recompileAll();
  }

  _isEditing() {
    return this._manualEditing || this.value.trim() === "";
  }

  _syncEditingClass() {
    this.classList.toggle("review-box--editing", this._isEditing());
  }

  _enterEditing() {
    if (this._manualEditing) return;
    this._manualEditing = true;
    this._draftResult = this._committedResult;
    this._syncEditingClass();
    this._renderDraftPreview();
    this._recompileDraft();
  }

  // Synchronous end-to-end (registry lookup -> state mutation -> focus): no
  // rAF/setTimeout/await anywhere in this call chain, so this is safe to
  // call from within the original tap/keydown call stack on iOS.
  focusField() {
    this._enterEditing();
    openEditor = this;
    this._codeField.focusField();
  }

  close() {
    this._manualEditing = false;
    if (openEditor === this) openEditor = null;
    this._syncEditingClass();
    this._commit();
  }

  _commit() {
    const newSource = this._codeField.value;
    if (newSource === this._lastCommittedSource) return;
    this._lastCommittedSource = newSource;
    this._committedResult = this._draftResult;
    this._renderCommittedPreview();
    this.dispatchEvent(new CustomEvent("tide-note-committed", { detail: { value: newSource }, bubbles: true, composed: true }));
  }

  _build() {
    this._built = true;
    this.classList.add("review-box");

    const editWrap = document.createElement("div");
    editWrap.className = "editable-typst-edit";

    const codeField = document.createElement("typst-code-field");
    codeField.setAttribute("placeholder", this.getAttribute("placeholder") ?? "");
    const nextField = this.getAttribute("next-field");
    if (nextField) codeField.setAttribute("next-field", nextField);
    const submitSelector = this.getAttribute("submit-selector");
    if (submitSelector) codeField.setAttribute("submit-selector", submitSelector);

    const initialSource = this.getAttribute("source") ?? "";
    codeField.setAttribute("source", initialSource);
    this._lastCommittedSource = initialSource;

    const draftPreview = document.createElement("div");
    draftPreview.className = "note-editor-preview";
    draftPreview.style.display = "none";

    editWrap.append(codeField, draftPreview);

    const previewLayer = document.createElement("div");
    previewLayer.className = "editable-typst-preview-layer";
    const committedPreviewSlot = document.createElement("div");
    committedPreviewSlot.style.display = "contents";
    const hint = document.createElement("span");
    hint.className = "editable-typst-hint";
    hint.textContent = this.getAttribute("shortcut-hint") ?? "";
    previewLayer.append(committedPreviewSlot, hint);

    this.replaceChildren(editWrap, previewLayer);

    this._codeField = codeField;
    this._draftPreview = draftPreview;
    this._committedPreviewSlot = committedPreviewSlot;
    this._hintEl = hint;

    codeField.addEventListener("typst-input", (event) => {
      this._syncEditingClass();
      this._recompileDraft();
      this.dispatchEvent(new CustomEvent("tide-note-input", { detail: { value: event.detail.value }, bubbles: true, composed: true }));
    });

    codeField.addEventListener("typst-blur", () => {
      this.close();
    });

    codeField.addEventListener("typst-image", (event) => {
      const { id, data } = event.detail;
      this._pendingImages = { ...this._pendingImages, [id]: data };
      this._recompileDraft();
      this.dispatchEvent(new CustomEvent("tide-image-added", { detail: { id, data }, bubbles: true, composed: true }));
    });

    // `focusin` (unlike `focus`) bubbles, so this also covers Tab-focus and
    // any other native way the inner textarea gains focus, not just an
    // explicit call to focusField().
    this.addEventListener("focusin", () => {
      this._enterEditing();
      openEditor = this;
    });

    // The safe zone for tap-outside-to-close is this whole box, not just
    // the field: a tap on, say, the draft-preview strip below the textarea
    // still bubbles a click up to this same box, so guarding on `isEditing`
    // here (rather than always focusing) avoids a flash-close-then-reopen.
    this.addEventListener("click", () => {
      if (!this._isEditing()) this.focusField();
    });

    this._syncEditingClass();
    this._recompileAll();
  }

  _recompileAll() {
    this._recompileCommitted();
    if (this._isEditing()) this._recompileDraft();
  }

  async _recompileCommitted() {
    const source = this._lastCommittedSource ?? "";
    const token = ++this._committedToken;
    if (source.trim() === "") {
      this._committedResult = null;
      this._renderCommittedPreview();
      return;
    }
    const result = await this._compile(source);
    if (token !== this._committedToken) return;
    this._committedResult = result;
    this._renderCommittedPreview();
  }

  async _recompileDraft() {
    const source = this._codeField.value;
    const token = ++this._draftToken;
    if (source.trim() === "") {
      this._draftResult = null;
      this._renderDraftPreview();
      return;
    }
    const result = await this._compile(source);
    if (token !== this._draftToken) return;
    this._draftResult = result;
    this._renderDraftPreview();
  }

  async _compile(source) {
    const preamble = this.getAttribute("preamble") ?? "";
    const theme = this.getAttribute("theme");
    const images = imageAttachments(this._knownImages, this._pendingImages, source);
    const [status, output] = await window.__TAURI__.core.invoke("render_typst", {
      rawTypst: source,
      ink: inkFor(theme),
      preamble,
      images,
      widthPt: this._widthPt(),
      textSizePt: cardTextSizePt(),
    });
    return { ok: status === 0, output };
  }

  // Typst compiles to a fixed page width, so the SVG has to be generated at
  // roughly the right size to begin with — measuring this element's own
  // rendered width (rather than a shared ancestor, since this element
  // exists and is sized correctly by the time this runs) lets Typst
  // re-lay-out the text at a size that's actually right for the screen.
  _widthPt() {
    const widthPx = this.clientWidth || 340.157480315 / 0.75;
    return widthPx * 0.75; // CSS px -> pt (96dpi: 1pt = 4/3 px)
  }

  _previewNode(result) {
    const r = result ?? { ok: false, output: "" };
    if (r.ok) {
      const img = document.createElement("img");
      img.src = svgDataUrl(r.output);
      return img;
    }
    const div = document.createElement("div");
    div.className = "note-editor-error";
    div.textContent = r.output;
    return div;
  }

  _renderCommittedPreview() {
    this._committedPreviewSlot.replaceChildren(this._previewNode(this._committedResult));
  }

  _renderDraftPreview() {
    if (!this._draftResult) {
      this._draftPreview.style.display = "none";
      this._draftPreview.replaceChildren();
      return;
    }
    this._draftPreview.style.display = "";
    this._draftPreview.replaceChildren(this._previewNode(this._draftResult));
  }
}

customElements.define("typst-note-editor", TypstNoteEditor);
