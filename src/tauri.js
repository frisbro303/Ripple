const inkFor = (darkMode) => (darkMode ? "#e8e9ec" : "#1c1e21");

function isDarkMode() {
  const theme = document.documentElement.getAttribute("data-theme");
  if (theme === "light") return false;
  if (theme === "dark") return true;
  return window.matchMedia("(prefers-color-scheme: dark)").matches;
}

// Typst compiles to a fixed page width, so the resulting SVG has to be
// generated at roughly the right size to begin with — image-scaling it
// down afterward to fit a narrow phone screen just shrinks the text along
// with it, which reads fine on a wide desktop window but becomes too small
// to comfortably read on a phone. Measuring the container's rendered width
// and converting it to points lets Typst re-lay-out the text at a size
// that's actually right for the screen it's on.
//
// This deliberately queries the shared `.review-card-area` wrapper instead
// of the specific field by requestId: a compile request is fired from
// EditableTypst's `init`, in the same Elm update cycle that creates that
// field's id attribute — Elm doesn't patch the id into the real DOM until
// the next animation frame, so looking it up immediately would almost
// always miss and silently fall back to the default width. `.review-box`
// is always 100% of `.review-card-area`, and that wrapper (present on both
// the Review and Add pages) is already on screen before any individual
// card's fields are, so it's there to measure right away.
function cardWidthPt() {
  const area = document.querySelector(".review-card-area");
  const widthPx = area ? area.clientWidth : 340.157480315 / 0.75;
  return widthPx * 0.75; // CSS px -> pt (96dpi: 1pt = 4/3 px)
}

// A touch device is held closer and viewed at a higher pixel density than
// a desktop monitor, so the same absolute point size that reads fine on
// desktop feels small on a phone — this isn't something wrapping at the
// right width fixes, the base size itself needs to be bigger. 14pt mirrors
// the existing Rust-side default; 17pt matches iOS's own standard body
// text size.
function cardTextSizePt() {
  return window.matchMedia("(pointer: coarse)").matches ? 17 : 14;
}

// "i"/"o" are global shortcuts (Main.elm's handleAddKey/handleReviewKey)
// that focus the front/back editor field as a side effect. Elm's
// `Browser.Events.onKeyDown` subscription is a passive listener — it can't
// call `preventDefault()` — so the same keydown's default browser action
// (inserting the typed character) still runs afterward, and since the
// field has by then already been synchronously focused via the
// `focusField` port, that default character insertion lands in the
// field that was *just* opened instead of doing nothing. Since this only
// happens when the key wasn't already headed for a text field, this
// listener only needs to suppress the default for "i"/"o" when the
// current target isn't already an input/textarea.
document.addEventListener("keydown", (event) => {
  const isTextInput = event.target.tagName === "INPUT" || event.target.tagName === "TEXTAREA";
  if (!isTextInput && !event.metaKey && !event.ctrlKey && (event.key === "i" || event.key === "o")) {
    event.preventDefault();
  }
});

// Remembers each field's cursor position across a blur/refocus cycle, keyed
// by textarea id. The textarea itself stays permanently mounted, so a
// browser's own selectionStart/End would normally just survive a blur on
// its own — but `focusField` below forces the cursor to the end every time
// it focuses a field (needed the first time a field is opened, so typing
// continues after any existing content rather than landing at position 0),
// which was overwriting that natural persistence on every subsequent
// re-entry too, so the cursor always "respawned" at the end instead of
// wherever the user had actually left it.
const lastSelection = new Map();

document.addEventListener(
  "blur",
  (event) => {
    const el = event.target;
    if (el instanceof HTMLTextAreaElement && el.classList.contains("note-editor-field")) {
      lastSelection.set(el.id, { start: el.selectionStart, end: el.selectionEnd });
    }
  },
  true
);

export function setupTauri(app) {
  app.ports.compileTypstPort.subscribe(async ({ requestId, source: rawTypst, preamble, images }) => {
    const [status, output] = await window.__TAURI__.core.invoke("render_typst", {
      rawTypst,
      ink: inkFor(isDarkMode()),
      preamble,
      images,
      widthPt: cardWidthPt(),
      textSizePt: cardTextSizePt(),
    });
    app.ports.rawTypstCompiledPort.send([requestId, status, output]);
  });

  app.ports.highlightTypstPort.subscribe(async ([requestId, source]) => {
    const tree = await window.__TAURI__.core.invoke("highlight_typst", { source });
    app.ports.typstHighlightedPort.send([requestId, tree]);
  });

  app.ports.focusField.subscribe((id) => {
    // Called synchronously (no requestAnimationFrame/setTimeout) so this
    // stays within the original tap's call stack — on iOS, .focus() only
    // raises the on-screen keyboard when it runs inside that same
    // synchronous window. This relies on the target textarea already
    // existing in the DOM (EditableTypst keeps it permanently mounted,
    // toggling visibility via CSS instead of conditionally rendering it)
    // rather than waiting for Elm to patch it into existence.
    //
    // Existing isn't enough on its own, though: the textarea is hidden via
    // `display: none` (through the `.review-box--editing` class, added by
    // Elm's own re-render) until editing actually starts, and a
    // `display: none` element can't receive focus at all — calling
    // `.focus()` on it is a silent no-op. Elm's virtual-dom patch that adds
    // that class is also deferred to the next animation frame, same as
    // element creation would be, so relying on it to have already run
    // reintroduces the exact race this function exists to avoid (most
    // visible when entering edit mode via a keyboard shortcut rather than
    // clicking directly on the field, since nothing else forces a
    // synchronous re-render first). Adding the class here directly makes
    // the field visible/focusable immediately; Elm adds the same class
    // moments later regardless, which is a harmless no-op by then.
    const el = document.getElementById(id);
    if (el) {
      const box = el.closest(".review-box");
      if (box) box.classList.add("review-box--editing");
      el.focus();
      const saved = lastSelection.get(id);
      if (saved && saved.end <= el.value.length) {
        el.setSelectionRange(saved.start, saved.end);
      } else {
        el.setSelectionRange(el.value.length, el.value.length);
      }
    }
  });

  app.ports.setPort.subscribe(async ({ key, value }) => {
    await window.__TAURI__.core.invoke("store_set", { key, value });
  });

  app.ports.deletePort.subscribe(async (key) => {
    await window.__TAURI__.core.invoke("store_delete", { key });
  });

  app.ports.getPort.subscribe(async (key) => {
    const value = await window.__TAURI__.core.invoke("store_get", { key });
    app.ports.loadedPort.send({ key, value: value ?? null });
  });

  app.ports.insertOpsPort.subscribe(async (ops) => {
    await window.__TAURI__.core.invoke("db_insert_ops", { ops });
  });

  app.ports.requestOpsPort.subscribe(async () => {
    const ops = await window.__TAURI__.core.invoke("db_get_ops");
    app.ports.opsLoadedPort.send(ops);
  });

  app.ports.clearOpsPort.subscribe(async () => {
    await window.__TAURI__.core.invoke("db_clear_ops");
  });

  app.ports.setThemePort.subscribe((theme) => {
    document.documentElement.setAttribute("data-theme", theme);
  });

  app.ports.exportDataPort.subscribe(async (json) => {
    const blob = new Blob([json], { type: "application/json" });

    // iOS Safari/WKWebView ignores the `download` attribute on `<a>`
    // entirely, so the blob-download trick below silently does nothing
    // there. The Web Share API is what actually works on touch devices —
    // it opens the native share sheet (Files, AirDrop, Mail, etc). Desktop
    // keeps the plain download, which is the more expected behavior there.
    const isTouch = window.matchMedia("(pointer: coarse)").matches;
    const file = new File([blob], "tide-backup.json", { type: "application/json" });

    if (isTouch && navigator.canShare && navigator.canShare({ files: [file] })) {
      try {
        await navigator.share({ files: [file] });
      } catch (err) {
        // User cancelled the share sheet — nothing else to do.
      }
      return;
    }

    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = "tide-backup.json";
    a.click();
    URL.revokeObjectURL(url);
  });

  app.ports.requestImportPort.subscribe(() => {
    const input = document.createElement("input");
    input.type = "file";
    input.accept = "application/json";
    input.onchange = () => {
      const file = input.files[0];
      if (!file) return;
      const reader = new FileReader();
      reader.onload = () => {
        app.ports.importLoadedPort.send(reader.result);
      };
      reader.readAsText(file);
    };
    input.click();
  });
}
