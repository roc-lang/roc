// Global state
let wasmModule = null;
let wasmMemory = null;
let currentState = "START";
let currentView = "diagnostics";
let lastDiagnostics = null;
let activeExample = null;

// Example modules
const examples = [
  {
    id: "hello-world",
    title: "Hello World",
    description: "Hello World application example",
    code: `app [main!] { pf: platform \"../basic-cli/platform.roc\" }

import pf.Stdout

main! = |_| Stdout.line!(\"Hello, world!\")`,
  },
  {
    id: "basic-types",
    title: "Basic Types",
    description: "Numbers, strings, and booleans",
    code: `module [name, age, height, isActive]

name : Str
name = "Alice"

age : I32
age = 25

height : Dec
height = 5.8

isActive : Bool
isActive = Bool.True`,
  },
];

// Initialize the playground
async function initializePlayground() {
  logInfo("Initializing playground...");
  try {
    setStatus("loading", "Loading WASM module...");
    logInfo("Loading WASM module...");
    await loadWasm();
    logInfo("WASM module loaded successfully");

    setStatus("loading", "Initializing compiler...");
    logInfo("Sending INIT message to WASM...");
    const response = await sendMessage({ type: "INIT" });
    logInfo("INIT response:", response);

    if (response.status !== "SUCCESS") {
      throw new Error(
        `WASM initialization failed: ${response.message || "Unknown error"}`,
      );
    }

    setStatus("loading", "Setting up examples...");
    logInfo("Populating examples...");
    populateExamples();
    logInfo("Updating UI...");
    updateUI();
    setStatus("ready", "Ready to compile Roc code!");
    logInfo("Playground initialization complete!");
  } catch (error) {
    logError("❌ Failed to initialize playground:", error);
    setStatus("error", `Initialization failed: ${error.message}`);
    showError(`Failed to initialize playground: ${error.message}`);
  }
}

// Load WASM module
async function loadWasm() {
  try {
    logInfo("Fetching WASM file...");
    const response = await fetch("playground.wasm");
    logInfo("WASM fetch response status:", response.status);

    if (!response.ok) {
      throw new Error(
        `Failed to fetch WASM file: ${response.status} ${response.statusText}`,
      );
    }

    logInfo("Converting to array buffer...");
    const bytes = await response.arrayBuffer();
    logInfo("WASM file size:", bytes.byteLength, "bytes");

    if (bytes.byteLength === 0) {
      throw new Error("WASM file is empty");
    }

    logInfo("Instantiating WASM module...");
    const module = await WebAssembly.instantiate(bytes, {
      env: {
        // Add any required imports here
      },
    });
    logInfo("WASM module instantiated");

    wasmModule = module.instance.exports;
    wasmMemory = wasmModule.memory;
    logInfo("WASM memory size:", wasmMemory.buffer.byteLength, "bytes");
    logInfo("Available WASM exports:", Object.keys(wasmModule));

    // Verify required exports are present
    const requiredExports = [
      "init",
      "processMessage",
      "allocate",
      "deallocate",
    ];
    for (const exportName of requiredExports) {
      if (typeof wasmModule[exportName] !== "function") {
        throw new Error(`Missing required WASM export: ${exportName}`);
      }
    }

    logInfo("Calling WASM init()...");
    wasmModule.init();
    logInfo("WASM init() completed");

    const outputContent = document.getElementById("outputContent");
    outputContent.innerHTML = "Ready to compile!";
    outputContent.classList.add("status-text");
  } catch (error) {
    logError("Error loading WASM:", error);
    throw new Error(`Failed to load WASM module: ${error.message}`);
  }
}

// Send message to WASM module
async function sendMessage(message) {
  if (!wasmModule) {
    throw new Error("WASM module not loaded");
  }

  logInfo("Sending message to WASM:", message);

  let messagePtr = null;
  let responsePtr = null;
  let messageBytes = null;

  try {
    const messageStr = JSON.stringify(message);
    messageBytes = new TextEncoder().encode(messageStr);
    logInfo("Message size:", messageBytes.length, "bytes");

    // Allocate memory for message
    logInfo("Allocating message memory...");
    messagePtr = wasmModule.allocate(messageBytes.length);
    if (!messagePtr) {
      throw new Error("Failed to allocate message memory");
    }

    const memory = new Uint8Array(wasmMemory.buffer);
    memory.set(messageBytes, messagePtr);
    logInfo("Message pointer:", messagePtr);

    // Allocate memory for response
    logInfo("Allocating response memory...");
    const responseBufferSize = 64 * 1024; // 64KB buffer
    responsePtr = wasmModule.allocate(responseBufferSize);
    if (!responsePtr) {
      throw new Error("Failed to allocate response memory");
    }
    logInfo("Response pointer:", responsePtr);

    // Process message
    logInfo("Processing message in WASM...");
    const responseLen = wasmModule.processMessage(
      messagePtr,
      messageBytes.length,
      responsePtr,
      responseBufferSize,
    );
    logInfo("Response length:", responseLen, "bytes");

    if (responseLen === 0) {
      throw new Error("WASM returned empty response");
    }

    // Read response
    const responseBytes = new Uint8Array(
      wasmMemory.buffer,
      responsePtr,
      responseLen,
    );
    const responseStr = new TextDecoder().decode(responseBytes);
    logInfo("Raw response:", responseStr);

    if (!responseStr.trim()) {
      throw new Error("WASM returned empty response string");
    }

    let parsedResponse;
    try {
      parsedResponse = JSON.parse(responseStr);
    } catch (jsonError) {
      logError("❌ JSON parse error:", jsonError);
      throw new Error(
        `Invalid JSON response from WASM: ${responseStr.substring(0, 100)}...`,
      );
    }

    logInfo("Parsed response:", parsedResponse);
    return parsedResponse;
  } catch (error) {
    logError("Error in sendMessage:", error);
    throw error;
  } finally {
    // Clean up memory
    if (messagePtr && wasmModule.deallocate && messageBytes) {
      wasmModule.deallocate(messagePtr, messageBytes.length);
    }
    if (responsePtr && wasmModule.deallocate) {
      wasmModule.deallocate(responsePtr, 64 * 1024);
    }
    logInfo("Memory cleaned up");
  }
}

// Populate examples list
function populateExamples() {
  const examplesList = document.getElementById("examplesList");
  examplesList.innerHTML = "";

  examples.forEach((example) => {
    const item = document.createElement("div");
    item.className = "example-item";
    item.dataset.exampleId = example.id;
    item.onclick = () => loadExample(example.id);

    item.innerHTML = `
            <div class="example-title">${example.title}</div>
            <div class="example-description">${example.description}</div>
        `;

    examplesList.appendChild(item);
  });
}

// Load an example
async function loadExample(exampleId) {
  logInfo("Loading example:", exampleId);
  const example = examples.find((e) => e.id === exampleId);
  if (!example) {
    logWarn("Example not found:", exampleId);
    return;
  }

  // Update UI
  logInfo("Updating example selection UI...");
  document.querySelectorAll(".example-item").forEach((item) => {
    item.classList.remove("active");
  });
  const activeItem = document.querySelector(`[data-example-id="${exampleId}"]`);
  if (activeItem) {
    activeItem.classList.add("active");
    logInfo("Activated example item");
  } else {
    logWarn("Could not find example item in DOM");
  }

  // Load code into editor
  logInfo("Loading code into editor...");
  document.getElementById("editor").value = example.code;
  activeExample = exampleId;

  // Reset if we're in loaded state
  if (currentState === "LOADED") {
    logInfo("Resetting WASM state...");
    await sendMessage({ type: "RESET" });
  }

  updateUI();
  logInfo("Example loaded successfully");
}

// Compile code
async function compileCode() {
  logInfo("Starting compilation...");
  const editor = document.getElementById("editor");
  const code = editor.value.trim();
  logInfo("Code length:", code.length, "characters");

  if (!code) {
    logWarn("No code to compile");
    showError("Please enter some code to compile");
    return;
  }

  try {
    logInfo("Beginning compilation process...");
    setStatus("loading", "Compiling...");
    disableButtons();

    // Reset if we're already in LOADED state
    if (currentState === "LOADED") {
      logInfo("Resetting WASM state before recompilation...");
      await sendMessage({ type: "RESET" });
    }

    const response = await sendMessage({
      type: "LOAD_SOURCE",
      source: code,
    });

    if (response.status === "SUCCESS") {
      logInfo("Compilation successful");
      currentState = "LOADED";
      lastDiagnostics = response.diagnostics;
      logInfo("Diagnostics:", lastDiagnostics);
      setStatus("loaded", "Code compiled");
      showDiagnostics();
    } else {
      logError("❌ Compilation failed:", response.message);
      showError(`Compilation failed: ${response.message}`);
    }
  } catch (error) {
    logError("Error during compilation:", error);
    showError(`Error during compilation: ${error.message}`);
  } finally {
    updateUI();
    logInfo("Compilation process finished");
  }
}

// Show diagnostics
function showDiagnostics() {
  currentView = "diagnostics";
  updateStageButtons();

  if (!lastDiagnostics) {
    showMessage("No diagnostics available");
    return;
  }

  const outputContent = document.getElementById("outputContent");
  let html = "";

  let totalErrors = 0;
  let totalWarnings = 0;

  // Count total diagnostics
  Object.values(lastDiagnostics).forEach((stageDiagnostics) => {
    stageDiagnostics.forEach((diagnostic) => {
      if (diagnostic.severity === "error" || diagnostic.severity === "fatal") {
        totalErrors++;
      } else if (diagnostic.severity === "warning") {
        totalWarnings++;
      }
    });
  });

  // Show summary
  if (totalErrors === 0 && totalWarnings === 0) {
    html += '<div class="success-message">✓ No issues found!</div>';
  } else {
    html += `<div class="diagnostic-summary">
            Found ${totalErrors} error(s) and ${totalWarnings} warning(s)
        </div>`;
  }

  // Show diagnostics by stage
  Object.entries(lastDiagnostics).forEach(([stage, diagnostics]) => {
    if (diagnostics.length > 0) {
      html += `<div class="diagnostic-stage">
                    <div class="diagnostic-stage-title">${stage.toUpperCase()}</div>`;

      diagnostics.forEach((diagnostic) => {
        html += `<div class="diagnostic ${diagnostic.severity}">
                        <div class="diagnostic-severity">${diagnostic.severity.toUpperCase()}</div>
                        <div class="diagnostic-message">${escapeHtml(diagnostic.title)}</div>
                    </div>`;
      });

      html += "</div>";
    }
  });

  outputContent.innerHTML = html;
}

// Show tokens
async function showTokens() {
  currentView = "tokens";
  updateStageButtons();

  try {
    const response = await sendMessage({
      type: "QUERY_TOKENS",
    });
    if (response.status === "SUCCESS") {
      showSExpression(response.data);
    } else {
      showError(`Failed to get tokens: ${response.message}`);
    }
  } catch (error) {
    showError(`Error getting tokens: ${error.message}`);
  }
}

// Show parse AST
async function showParseAst() {
  currentView = "parse";
  updateStageButtons();

  try {
    const response = await sendMessage({
      type: "QUERY_AST",
    });
    if (response.status === "SUCCESS") {
      showSExpression(response.data);
    } else {
      showError(`Failed to get parse AST: ${response.message}`);
    }
  } catch (error) {
    showError(`Error getting parse AST: ${error.message}`);
  }
}

// Show CIR
async function showCanCir() {
  currentView = "can";
  updateStageButtons();

  try {
    const response = await sendMessage({
      type: "QUERY_CIR",
    });
    if (response.status === "SUCCESS") {
      showSExpression(response.data);
    } else {
      showError(`Failed to get CIR: ${response.message}`);
    }
  } catch (error) {
    showError(`Error getting CIR: ${error.message}`);
  }
}

// Show types
async function showTypes() {
  currentView = "types";
  updateStageButtons();

  try {
    const response = await sendMessage({ type: "QUERY_TYPES" });
    if (response.status === "SUCCESS") {
      showSExpression(response.data);
    } else {
      showError(`Failed to get types: ${response.message}`);
    }
  } catch (error) {
    showError(`Error getting types: ${error.message}`);
  }
}

function showSExpression(sexp) {
  const outputContent = document.getElementById("outputContent");

  // Display the HTML S-expression directly
  outputContent.innerHTML = `<pre class="sexp-output">${sexp}</pre>`;
}

// Show error message
function showError(message) {
  const outputContent = document.getElementById("outputContent");
  outputContent.innerHTML = `<div class="error-message">${escapeHtml(message)}</div>`;
}

// Show general message
function showMessage(message) {
  const outputContent = document.getElementById("outputContent");
  outputContent.innerHTML = `<div class="loading">${escapeHtml(message)}</div>`;
}

// Update status indicator
function setStatus(status, text) {
  logInfo("Status update:", status, "-", text);
  const statusDot = document.getElementById("statusDot");
  const statusText = document.getElementById("statusText");

  statusDot.className = `status-dot ${status}`;
  statusText.textContent = text;

  if (status === "ready") {
    currentState = "READY";
    logInfo("State changed to READY");
  } else if (status === "loaded") {
    currentState = "LOADED";
    logInfo("State changed to LOADED");
  }
}

// Update UI based on current state
function updateUI() {
  const compileBtn = document.getElementById("compileBtn");
  const stageButtons = document.querySelectorAll(
    ".stage-button:not(#diagnosticsBtn)",
  );

  switch (currentState) {
    case "START":
      compileBtn.disabled = false;
      stageButtons.forEach((btn) => (btn.disabled = true));
      break;
    case "READY":
      compileBtn.disabled = false;
      stageButtons.forEach((btn) => (btn.disabled = true));
      break;
    case "LOADED":
      compileBtn.disabled = false;
      stageButtons.forEach((btn) => (btn.disabled = false));
      break;
  }
}

// Update stage buttons
function updateStageButtons() {
  document.querySelectorAll(".stage-button").forEach((btn) => {
    btn.classList.remove("active");
  });

  const activeBtn = {
    diagnostics: "diagnosticsBtn",
    tokens: "tokensBtn",
    parse: "parseBtn",
    can: "canBtn",
    types: "typesBtn",
  }[currentView];

  if (activeBtn) {
    document.getElementById(activeBtn).classList.add("active");
  }
}

// Disable buttons during operation
function disableButtons() {
  document.getElementById("compileBtn").disabled = true;
  document.querySelectorAll(".stage-button").forEach((btn) => {
    btn.disabled = true;
  });
}

// Escape HTML
function escapeHtml(text) {
  const div = document.createElement("div");
  div.textContent = text;
  return div.innerHTML;
}

// Handle keyboard shortcuts
document.addEventListener("keydown", function (e) {
  if ((e.ctrlKey || e.metaKey) && e.key === "Enter") {
    e.preventDefault();
    compileCode();
  }
});

// Handle resizable panels
let isResizing = false;
let startX = 0;
let startWidthLeft = 0;
let startWidthRight = 0;

const resizeHandle = document.getElementById("resizeHandle");
const editorContainer = document.querySelector(".editor-container");
const outputContainer = document.querySelector(".output-container");

resizeHandle.addEventListener("mousedown", (e) => {
  isResizing = true;
  startX = e.clientX;
  startWidthLeft = editorContainer.offsetWidth;
  startWidthRight = outputContainer.offsetWidth;
  document.body.style.cursor = "col-resize";
  e.preventDefault();
});

document.addEventListener("mousemove", (e) => {
  if (!isResizing) return;

  const diff = e.clientX - startX;
  const newWidthLeft = startWidthLeft + diff;
  const newWidthRight = startWidthRight - diff;

  // Enforce minimum widths
  if (newWidthLeft >= 300 && newWidthRight >= 300) {
    editorContainer.style.flex = `0 0 ${newWidthLeft}px`;
    outputContainer.style.flex = `0 0 ${newWidthRight}px`;
  }
});

document.addEventListener("mouseup", () => {
  if (isResizing) {
    isResizing = false;
    document.body.style.cursor = "default";
  }
});

// Theme handling
function initTheme() {
  const savedTheme = localStorage.getItem("theme");
  const systemPrefersDark = window.matchMedia(
    "(prefers-color-scheme: dark)",
  ).matches;

  // Use saved theme, or fall back to system preference
  const theme = savedTheme || (systemPrefersDark ? "dark" : "light");
  document.documentElement.setAttribute("data-theme", theme);

  // Update theme switch state
  const themeSwitch = document.getElementById("themeSwitch");
  themeSwitch.setAttribute("aria-checked", theme === "dark");
}

function toggleTheme() {
  const currentTheme = document.documentElement.getAttribute("data-theme");
  const newTheme = currentTheme === "dark" ? "light" : "dark";

  document.documentElement.setAttribute("data-theme", newTheme);
  localStorage.setItem("theme", newTheme);

  const themeSwitch = document.getElementById("themeSwitch");
  themeSwitch.setAttribute("aria-checked", newTheme === "dark");
}

function logInfo(msg) {
  console.log("INFO: " + msg);
}

function logWarn(msg) {
  console.warn("WARNING: " + msg);
}

function logError(msg) {
  console.error("ERROR: " + msg);
}

// Initialize theme on page load
initTheme();

// Theme switch event listener
document.getElementById("themeSwitch").addEventListener("click", toggleTheme);
document.getElementById("themeSwitch").addEventListener("keydown", (e) => {
  if (e.key === "Enter" || e.key === " ") {
    e.preventDefault();
    toggleTheme();
  }
});

// Initialize when page loads
logInfo("Page loaded, setting up initialization...");

window.addEventListener("load", initializePlayground);
