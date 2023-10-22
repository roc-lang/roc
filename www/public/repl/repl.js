// The only way we can provide values to wasm_bindgen's generated code is to set globals
window.js_create_app = js_create_app;
window.js_run_app = js_run_app;
window.js_get_result_and_memory = js_get_result_and_memory;

// The only place we use console.error is in wasm_bindgen, where it gets a single string argument.
console.error = function displayErrorInHistoryPanel(string) {
  const html = `<div class="panic">${string}</div>`;
  updateHistoryEntry(repl.inputHistoryIndex, false, html);
};

import * as roc_repl_wasm from "./roc_repl_wasm.js";

// ----------------------------------------------------------------------------
// REPL state
// ----------------------------------------------------------------------------

const repl = {
  elemHistory: document.getElementById("history-text"),
  elemSourceInput: document.getElementById("source-input"),

  inputQueue: [],
  inputHistory: [],
  inputHistoryIndex: 0,
  inputStash: "", // stash the user input while we're toggling through history with up/down arrows

  textDecoder: new TextDecoder(),
  textEncoder: new TextEncoder(),

  compiler: null,
  app: null,

  // Temporary storage for the address of the result of running the user's code.
  // Used while control flow returns to Rust to allocate space to copy the app's memory buffer.
  result_addr: 0,
};

// Initialise
repl.elemSourceInput.addEventListener("input", onInput);
repl.elemSourceInput.addEventListener("keydown", onInputKeydown);
repl.elemSourceInput.addEventListener("keyup", onInputKeyup);
roc_repl_wasm.default("/repl/roc_repl_wasm_bg.wasm").then(async (instance) => {
  repl.elemHistory.querySelector("#loading-message").remove();
  repl.elemSourceInput.disabled = false;
  repl.elemSourceInput.placeholder = "Type some Roc code and press Enter.";
  repl.elemSourceInput.focus();
  repl.compiler = instance;

  // Get help text from the compiler, and display it at top of the history panel
  try {
    const helpText = await roc_repl_wasm.entrypoint_from_js(":help");
    const helpElem = document.getElementById("help-text");
    helpElem.innerHTML = helpText.trim();
  } catch (e) {
    // Print error for Roc devs. Don't use console.error, we overrode that above to display on the page!
    console.warn(e);
  }
});

// ----------------------------------------------------------------------------
// Handle inputs
// ----------------------------------------------------------------------------

function onInput(event) {
  // Have the textarea grow with the input
  event.target.style.height = event.target.scrollHeight + 2 + "px"; // +2 for the border
}

function onInputKeydown(event) {
  const ENTER = 13;

  const { keyCode } = event;

  if (keyCode === ENTER) {
    if (!event.shiftKey && !event.ctrlKey && !event.altKey) {
      // Don't advance the caret to the next line
      event.preventDefault();

      const inputText = repl.elemSourceInput.value.trim();

      repl.elemSourceInput.value = "";
      repl.elemSourceInput.style.height = "";

      repl.inputQueue.push(inputText);
      if (repl.inputQueue.length === 1) {
        processInputQueue();
      }
    }
  }
}

function onInputKeyup(event) {
  const UP = 38;
  const DOWN = 40;

  const { keyCode } = event;

  const el = repl.elemSourceInput;

  switch (keyCode) {
    case UP:
      if (repl.inputHistory.length === 0) {
        return;
      }
      if (repl.inputHistoryIndex == repl.inputHistory.length - 1) {
        repl.inputStash = el.value;
      }
      setInput(repl.inputHistory[repl.inputHistoryIndex]);

      if (repl.inputHistoryIndex > 0) {
        repl.inputHistoryIndex--;
      }
      break;

    case DOWN:
      if (repl.inputHistory.length === 0) {
        return;
      }
      if (repl.inputHistoryIndex === repl.inputHistory.length - 1) {
        setInput(repl.inputStash);
      } else {
        repl.inputHistoryIndex++;
        setInput(repl.inputHistory[repl.inputHistoryIndex]);
      }
      break;

    default:
      break;
  }
}

function setInput(value) {
  const el = repl.elemSourceInput;
  el.value = value;
  el.selectionStart = value.length;
  el.selectionEnd = value.length;
}

// Use a queue just in case we somehow get inputs very fast
// We want the REPL to only process one at a time, since we're using some global state.
// In normal usage we shouldn't see this edge case anyway. Maybe with copy/paste?
async function processInputQueue() {
  while (repl.inputQueue.length) {
    const inputText = repl.inputQueue[0];
    repl.inputHistoryIndex = createHistoryEntry(inputText);
    repl.inputStash = "";

    let outputText = "";
    let ok = true;
    if (inputText) {
      try {
        outputText = await roc_repl_wasm.entrypoint_from_js(inputText);
      } catch (e) {
        outputText = `${e}`;
        ok = false;
      }
    }

    updateHistoryEntry(repl.inputHistoryIndex, ok, outputText);
    repl.inputQueue.shift();
  }
}

// ----------------------------------------------------------------------------
// Callbacks to JS from Rust
// ----------------------------------------------------------------------------

// Load Wasm code into the browser's virtual machine, so we can run it later.
// This operation is async, so we call it before entering any code shared
// with the command-line REPL, which is sync.
async function js_create_app(wasm_module_bytes) {
  const { instance } = await WebAssembly.instantiate(wasm_module_bytes);
  // Keep the instance alive so we can run it later from shared REPL code
  repl.app = instance;
}

// Call the `main` function of the user app, via the `wrapper` function.
function js_run_app() {
  const { wrapper, memory } = repl.app.exports;

  // Run the user code, and remember the result address
  // We'll pass it to Rust in the next callback
  repl.result_addr = wrapper();

  // Tell Rust how much space to reserve for its copy of the app's memory buffer.
  // We couldn't know that size until we actually ran the app.
  return memory.buffer.byteLength;
}

// After Rust has allocated space for the app's memory buffer,
// we copy it, and return the result address too
function js_get_result_and_memory(buffer_alloc_addr) {
  const appMemory = new Uint8Array(repl.app.exports.memory.buffer);
  const compilerMemory = new Uint8Array(repl.compiler.memory.buffer);
  compilerMemory.set(appMemory, buffer_alloc_addr);
  return repl.result_addr;
}

// ----------------------------------------------------------------------------
// Rendering
// ----------------------------------------------------------------------------

function createHistoryEntry(inputText) {
  const historyIndex = repl.inputHistory.length;
  repl.inputHistory.push(inputText);

  const firstLinePrefix = '<span class="input-line-prefix">» </span>';
  const otherLinePrefix = '<br><span class="input-line-prefix">… </span>';
  const inputLines = inputText.split("\n");
  if (inputLines[inputLines.length - 1] === "") {
    inputLines.pop();
  }
  const inputWithPrefixes = firstLinePrefix + inputLines.join(otherLinePrefix);

  const inputElem = document.createElement("div");
  inputElem.innerHTML = inputWithPrefixes;
  inputElem.classList.add("input");

  const historyItem = document.createElement("div");
  historyItem.appendChild(inputElem);
  historyItem.classList.add("history-item");

  repl.elemHistory.appendChild(historyItem);

  return historyIndex;
}

function updateHistoryEntry(index, ok, outputText) {
  const outputElem = document.createElement("div");
  outputElem.innerHTML = outputText;
  outputElem.classList.add("output", ok ? "output-ok" : "output-error");

  const historyItem = repl.elemHistory.children[index];
  historyItem.appendChild(outputElem);

  // Scroll the page to the bottom so you can see the most recent output.
  window.scrollTo(0, document.body.scrollHeight);
}
