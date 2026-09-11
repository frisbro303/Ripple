import { getFocusable } from "./Typst/code-field.js";

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

// Non-editor, non-input chrome shouldn't show the browser's native context
// menu; editor fields (<typst-code-field>) handle their own context menu
// (opening the image picker) directly, and plain inputs keep their native
// menu (e.g. for cut/copy/paste in a number field).
document.addEventListener("contextmenu", (event) => {
  const el = event.target;
  if (el instanceof HTMLInputElement || el instanceof HTMLTextAreaElement) return;
  event.preventDefault();
});

export function setupTauri(app) {
  app.ports.focusField.subscribe((id) => {
    // Looked up in a registry populated at real-connect time (see
    // Typst/code-field.js), so this can never be stale or missing because
    // Elm hasn't patched the DOM yet — and `.focusField()` itself is fully
    // synchronous (no rAF/setTimeout/await), so this stays within the
    // original keydown's call stack, which iOS requires to raise the
    // on-screen keyboard.
    const el = getFocusable(id);
    if (el && typeof el.focusField === "function") el.focusField();
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
