let wasm;
const MAX_MODULES = 16;
const decoder = new TextDecoder();
const encoder = new TextEncoder();
const outputEl = document.getElementById("output");
const errorsEl = document.getElementById("errors");
const modulesListEl = document.getElementById("modules-list");
const addModuleBtn = document.getElementById("add-module");
const moduleTemplate = document.getElementById("module-template");

const importObject = {
  env: {
    js_echo(ptr, len) {
      const bytes = new Uint8Array(wasm.instance.exports.memory.buffer, ptr, len);
      outputEl.textContent += decoder.decode(bytes) + "\n";
    },
    js_stderr(ptr, len) {
      const bytes = new Uint8Array(wasm.instance.exports.memory.buffer, ptr, len);
      errorsEl.innerHTML += decoder.decode(bytes);
    },
  },
};

function appendTrapNotice(err) {
  const div = document.createElement("div");
  div.className = "report error";
  const h = document.createElement("h1");
  h.textContent = "Compiler crashed";
  const p = document.createElement("pre");
  p.textContent =
    "The Roc compiler hit an unrecoverable error while compiling your code. " +
    "This is a bug in Roc — please report it.\n\n" +
    String(err);
  div.appendChild(h);
  div.appendChild(p);
  errorsEl.appendChild(div);
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
  errorsEl.innerHTML = "";

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

  if (code === 255 && !errorsEl.innerHTML) {
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
