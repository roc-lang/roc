// wasm_bindgen treats our `extern` declarations as JS globals, so let's keep it happy
window.js_create_app = js_create_app;
window.js_run_app = js_run_app;
window.js_get_result_and_memory = js_get_result_and_memory;
import * as roc_repl_wasm from "./roc_repl_wasm.js";
import { getMockWasiImports } from "./wasi.js";

// ----------------------------------------------------------------------------
// REPL state
// ----------------------------------------------------------------------------

const repl = {
  elemHistory: document.getElementById("history-text"),
  elemSourceInput: document.getElementById("source-input"),

  inputQueue: [],
  inputHistory: [],

  textDecoder: new TextDecoder(),
  textEncoder: new TextEncoder(),

  compiler: null,
  app: null,

  // Temporary storage for values passing back and forth between JS and Wasm
  result: { addr: 0, buffer: new ArrayBuffer() },
};

// Initialise
repl.elemSourceInput.addEventListener("change", onInputChange);
roc_repl_wasm.default().then((instance) => {
  repl.compiler = instance;
});

// ----------------------------------------------------------------------------
// Handle inputs
// ----------------------------------------------------------------------------

function onInputChange(event) {
  const inputText = event.target.value;
  event.target.value = "";

  repl.inputQueue.push(inputText);
  if (repl.inputQueue.length === 1) {
    processInputQueue();
  }
}

// Use a queue just in case we somehow get inputs very fast
// We want the REPL to only process one at a time, since we're using some global state.
// In normal usage we shouldn't see this edge case anyway. Maybe with copy/paste?
async function processInputQueue() {
  while (repl.inputQueue.length) {
    const inputText = repl.inputQueue[0];
    const historyIndex = createHistoryEntry(inputText);

    let outputText;
    let ok = true;
    try {
      outputText = await roc_repl_wasm.entrypoint_from_js(inputText);
    } catch (e) {
      outputText = `${e}`;
      ok = false;
    }

    updateHistoryEntry(historyIndex, ok, outputText);
    repl.inputQueue.shift();
  }
}

// ----------------------------------------------------------------------------
// Callbacks to JS from Rust
// ----------------------------------------------------------------------------

// Create an executable Wasm instance from an array of bytes
// (Browser validates the module and does the final compilation to the host's machine code.)
async function js_create_app(wasm_module_bytes) {
  const wasiLinkObject = {}; // gives the WASI functions a reference to the app so they can write to its memory
  const importObj = getMockWasiImports(wasiLinkObject);
  const { instance } = await WebAssembly.instantiate(
    wasm_module_bytes,
    importObj
  );
  wasiLinkObject.instance = instance;
  repl.app = instance;
}

// Call the main function of the app, via the test wrapper
// Cache the result and return the size of the app's memory
function js_run_app() {
  const { wrapper, memory } = repl.app.exports;
  const addr = wrapper();
  const { buffer } = memory;
  repl.result = { addr, buffer };

  // Tell Rust how much space to reserve for its copy of the app's memory buffer.
  // This is not predictable, since the app can resize its own memory via malloc.
  return buffer.byteLength;
}

// After the Rust app has allocated space for the app's memory buffer,
// it calls this function and we copy it, and return the result too
function js_get_result_and_memory(buffer_alloc_addr) {
  const { addr, buffer } = repl.result;
  const appMemory = new Uint8Array(buffer);
  const compilerMemory = new Uint8Array(repl.compiler.memory.buffer);
  compilerMemory.set(appMemory, buffer_alloc_addr);
  return addr;
}

// ----------------------------------------------------------------------------
// Rendering
// ----------------------------------------------------------------------------

function createHistoryEntry(inputText) {
  const historyIndex = repl.inputHistory.length;
  repl.inputHistory.push(inputText);

  const inputElem = document.createElement("div");
  inputElem.textContent = "> " + inputText;
  inputElem.classList.add("input");

  const historyItem = document.createElement("div");
  historyItem.appendChild(inputElem);

  repl.elemHistory.appendChild(historyItem);
  repl.elemHistory.scrollTop = repl.elemHistory.scrollHeight;

  return historyIndex;
}

function updateHistoryEntry(index, ok, outputText) {
  const outputElem = document.createElement("div");
  outputElem.textContent = outputText;
  outputElem.classList.add("output");
  outputElem.classList.add(ok ? "output-ok" : "output-error");

  const historyItem = repl.elemHistory.childNodes[index];
  historyItem.appendChild(outputElem);

  repl.elemHistory.scrollTop = repl.elemHistory.scrollHeight;
}
