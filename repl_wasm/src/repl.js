// wasm_bindgen's JS code expects our imported functions to be global
window.js_create_and_run_app = js_create_and_run_app;
window.js_copy_app_memory = js_copy_app_memory;
import * as mock_repl from "./mock_repl.js";

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

  // Temporary storage for values passing back and forth between JS and Wasm
  result: { addr: 0, buffer: new ArrayBuffer() },
};

// Initialise
repl.elemSourceInput.addEventListener("change", onInputChange);
mock_repl.default().then((instance) => {
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
// This is definitely an edge case, but maybe could happen from copy/paste?
// It allows us to rely on having only one input at a time
async function processInputQueue() {
  while (repl.inputQueue.length) {
    const inputText = repl.inputQueue[0];
    const historyIndex = createHistoryEntry(inputText);

    let outputText;
    let ok = true;
    try {
      outputText = await mock_repl.webrepl_run(inputText);
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

// Transform the app bytes into a Wasm module and execute it
// Cache the result but don't send it to Rust yet, as it needs to allocate space first.
// Then it will immediately call js_copy_app_memory.
async function js_create_and_run_app(app_bytes, app_memory_size_ptr) {
  const { instance: app } = await WebAssembly.instantiate(app_bytes);

  const addr = app.exports.run();
  const { buffer } = app.exports.memory;
  repl.result = { addr, buffer };

  // Tell Rust how much space to reserve for its copy of the app's memory buffer.
  // The app might have grown its heap while running, so we couldn't predict it beforehand.
  // Write the result to memory instead of returning, since wasm_bindgen only allows
  // imported async functions to return JsValue, which has some conversion overhead.
  const compilerMemory32 = new Uint32Array(repl.compiler.memory.buffer);
  compilerMemory32[app_memory_size_ptr >> 2] = buffer.byteLength;
}

// Now that the Rust app has allocated space for the app's memory buffer, we can copy it
function js_copy_app_memory(buffer_alloc_addr) {
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
