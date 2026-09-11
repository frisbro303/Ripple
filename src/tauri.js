const inkFor = (darkMode) => (darkMode ? "#e8e9ec" : "#1c1e21");

function isDarkMode() {
  const theme = document.documentElement.getAttribute("data-theme");
  if (theme === "light") return false;
  if (theme === "dark") return true;
  return window.matchMedia("(prefers-color-scheme: dark)").matches;
}

const FALLBACK_CARD_WIDTH_PX = 340.157480315 / 0.75;
const PX_PER_PT = 0.75;

function cardWidthPt() {
  const area = document.querySelector(".review-card-area");
  const widthPx = area ? area.clientWidth : FALLBACK_CARD_WIDTH_PX;
  return widthPx * PX_PER_PT;
}

function cardTextSizePt() {
  return window.matchMedia("(pointer: coarse)").matches ? 17 : 14;
}

document.addEventListener("keydown", (event) => {
  const isTextInput = event.target.tagName === "INPUT" || event.target.tagName === "TEXTAREA";
  if (!isTextInput && !event.metaKey && !event.ctrlKey && (event.key === "i" || event.key === "o")) {
    event.preventDefault();
  }
});

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

function makeFieldVisible(el) {
  const box = el.closest(".review-box");
  if (box) box.classList.add("review-box--editing");
}

function restoreOrEndSelection(el, saved) {
  if (saved && saved.end <= el.value.length) {
    el.setSelectionRange(saved.start, saved.end);
  } else {
    el.setSelectionRange(el.value.length, el.value.length);
  }
}

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
    const el = document.getElementById(id);
    if (!el) return;
    makeFieldVisible(el);
    el.focus();
    restoreOrEndSelection(el, lastSelection.get(id));
  });

  app.ports.blurField.subscribe((id) => {
    const el = document.getElementById(id);
    if (el) el.blur();
  });

  app.ports.setSelectionPort.subscribe(({ id, start, end }) => {
    const el = document.getElementById(id);
    if (el && typeof el.setSelectionRange === "function") el.setSelectionRange(start, end);
  });

  app.ports.alert.subscribe((message) => {
    alert(message);
  });

  window.addEventListener("blur", () => {
    app.ports.windowFocusChanged.send(false);
  });
  window.addEventListener("focus", () => {
    app.ports.windowFocusChanged.send(true);
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

    const isTouch = window.matchMedia("(pointer: coarse)").matches;
    const file = new File([blob], "tide-backup.json", { type: "application/json" });

    if (isTouch && navigator.canShare && navigator.canShare({ files: [file] })) {
      try {
        await navigator.share({ files: [file] });
      } catch (err) {}
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
