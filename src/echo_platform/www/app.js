let wasm;
const MAX_MODULES = 16;
const decoder = new TextDecoder();
const encoder = new TextEncoder();
const outputEl = document.getElementById("output");
const errorsEl = document.getElementById("errors");
const modulesListEl = document.getElementById("modules-list");
const addModuleBtn = document.getElementById("add-module");
const moduleTemplate = document.getElementById("module-template");

const ANSI_FG_CLASSES = {
  30: "ansi-fg-black",
  31: "ansi-fg-red",
  32: "ansi-fg-green",
  33: "ansi-fg-yellow",
  34: "ansi-fg-blue",
  35: "ansi-fg-magenta",
  36: "ansi-fg-cyan",
  37: "ansi-fg-white",
  90: "ansi-fg-bright-black",
  91: "ansi-fg-bright-red",
  92: "ansi-fg-bright-green",
  93: "ansi-fg-bright-yellow",
  94: "ansi-fg-bright-blue",
  95: "ansi-fg-bright-magenta",
  96: "ansi-fg-bright-cyan",
  97: "ansi-fg-bright-white",
};

function resetAnsiState(state) {
  state.bold = false;
  state.dim = false;
  state.italic = false;
  state.underline = false;
  state.fg = "";
}

function freshAnsiState() {
  const state = {};
  resetAnsiState(state);
  return state;
}

const ansiState = freshAnsiState();

function applyAnsiSgr(params, state) {
  if (params.length === 0) params = [0];

  for (const code of params) {
    if (code === 0) {
      resetAnsiState(state);
    } else if (code === 1) {
      state.bold = true;
    } else if (code === 2) {
      state.dim = true;
    } else if (code === 3) {
      state.italic = true;
    } else if (code === 4) {
      state.underline = true;
    } else if (code === 22) {
      state.bold = false;
      state.dim = false;
    } else if (code === 23) {
      state.italic = false;
    } else if (code === 24) {
      state.underline = false;
    } else if (code === 39) {
      state.fg = "";
    } else if (ANSI_FG_CLASSES[code]) {
      state.fg = ANSI_FG_CLASSES[code];
    }
  }
}

function ansiClassName(state) {
  return [
    state.bold && "ansi-bold",
    state.dim && "ansi-dim",
    state.italic && "ansi-italic",
    state.underline && "ansi-underline",
    state.fg,
  ]
    .filter(Boolean)
    .join(" ");
}

function appendAnsiText(parent, text, state) {
  if (!text) return;

  const className = ansiClassName(state);
  if (!className) {
    parent.appendChild(document.createTextNode(text));
    return;
  }

  const span = document.createElement("span");
  span.className = className;
  span.textContent = text;
  parent.appendChild(span);
}

function appendAnsi(parent, text, state) {
  let index = 0;

  while (index < text.length) {
    const esc = text.indexOf("\u001b", index);
    if (esc === -1) {
      appendAnsiText(parent, text.slice(index), state);
      break;
    }

    appendAnsiText(parent, text.slice(index, esc), state);
    index = esc + 1;

    if (text[index] !== "[") continue;

    const finalIndex = text.slice(index + 1).search(/[@-~]/);
    if (finalIndex === -1) break;

    const end = index + 1 + finalIndex;
    if (text[end] === "m") {
      const body = text.slice(index + 1, end);
      const params = body
        ? body
            .split(";")
            .map((part) => (part === "" ? 0 : Number(part)))
            .filter((value) => Number.isInteger(value))
        : [0];
      applyAnsiSgr(params, state);
    }
    index = end + 1;
  }
}

function appendErrorsTerminal(text) {
  let terminal = errorsEl.querySelector(".terminal");
  if (!terminal) {
    terminal = document.createElement("pre");
    terminal.className = "terminal";
    errorsEl.appendChild(terminal);
  }
  appendAnsi(terminal, text, ansiState);
}

const importObject = {
  env: {
    js_echo(ptr, len) {
      const bytes = new Uint8Array(wasm.instance.exports.memory.buffer, ptr, len);
      outputEl.textContent += decoder.decode(bytes) + "\n";
    },
    js_stderr(ptr, len) {
      const bytes = new Uint8Array(wasm.instance.exports.memory.buffer, ptr, len);
      appendErrorsTerminal(decoder.decode(bytes));
    },
  },
};

function appendTrapNotice(err) {
  appendErrorsTerminal(
    "\u001b[1;31mCompiler crashed\u001b[0m\n" +
      "The Roc compiler hit an unrecoverable error while compiling your code. " +
      "This is a bug in Roc - please report it.\n\n" +
      String(err) +
      "\n",
  );
}

async function reinstantiate() {
  const instance = await WebAssembly.instantiate(wasm.module, importObject);
  wasm = { module: wasm.module, instance };
  wasm.instance.exports.init();
}

function wasmWrite(str) {
  const encoded = encoder.encode(str);
  const ptr = wasm.instance.exports.allocateBuffer(encoded.length);
  if (!ptr) throw new Error("allocateBuffer returned null");
  new Uint8Array(wasm.instance.exports.memory.buffer, ptr, encoded.length).set(encoded);
  return { ptr, len: encoded.length };
}

function updateAddButton() {
  addModuleBtn.disabled = modulesListEl.children.length >= MAX_MODULES;
}

function addModuleCard(name, content) {
  if (modulesListEl.children.length >= MAX_MODULES) return null;
  const card = moduleTemplate.content.firstElementChild.cloneNode(true);
  card.querySelector(".module-name").value = name;
  card.querySelector(".module-content").value = content;
  card.querySelector(".remove").addEventListener("click", () => {
    card.remove();
    updateAddButton();
  });
  modulesListEl.appendChild(card);
  updateAddButton();
  return card;
}

function appendSkipNotice(msg) {
  const div = document.createElement("div");
  div.className = "skip-notice";
  div.textContent = msg;
  errorsEl.appendChild(div);
}

async function run() {
  outputEl.textContent = "";
  errorsEl.textContent = "";
  resetAnsiState(ansiState);

  let trapped = false;
  let code = 255;
  try {
    wasm.instance.exports.init();

    for (const card of modulesListEl.querySelectorAll(".module-card")) {
      const name = card.querySelector(".module-name").value.trim();
      const content = card.querySelector(".module-content").value;
      if (!name) {
        appendSkipNotice("Skipped a module with an empty name.");
        continue;
      }
      const n = wasmWrite(name);
      const c = wasmWrite(content);
      wasm.instance.exports.addFile(n.ptr, n.len, c.ptr, c.len);
    }

    const s = wasmWrite(document.getElementById("source").value);
    code = wasm.instance.exports.compileAndRun(s.ptr, s.len);
  } catch (err) {
    trapped = true;
    appendTrapNotice(err);
  }

  if (trapped) {
    try {
      await reinstantiate();
    } catch (err) {
      appendTrapNotice(err);
    }
    return;
  }

  if (code === 255 && !errorsEl.textContent) {
    errorsEl.textContent = "Compilation or execution failed";
  } else if (code !== 0 && code !== 255) {
    outputEl.textContent += `Exit code: ${code}\n`;
  }
}

addModuleBtn.addEventListener("click", () => {
  addModuleCard("", "");
});

addModuleCard(
  "Greeting",
  `Greeting := [].{
    msg : Str
    msg = "Hello from the Greeting module!"
}`,
);

fetch("echo.wasm")
  .then((r) => WebAssembly.instantiateStreaming(r, importObject))
  .then((result) => {
    wasm = result;
    wasm.instance.exports.init();
    document.getElementById("run").addEventListener("click", run);
    document.getElementById("run").disabled = false;
  });
