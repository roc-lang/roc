// Global state
let wasmModule = null;
let wasmMemory = null;
let currentState = "START";
let currentView = "diagnostics";
let lastDiagnostics = null;
let activeExample = null;
let lastCompileTime = null;
let updateUrlTimeout = null;

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
    logInfo("Loading WASM module...");
    await loadWasm();
    logInfo("WASM module loaded successfully");

    logInfo("Sending INIT message to WASM...");
    const response = await sendMessage({ type: "INIT" });
    logInfo("INIT response:", response);

    if (response.status !== "SUCCESS") {
      throw new Error(
        `WASM initialization failed: ${response.message || "Unknown error"}`,
      );
    }

    logInfo("Populating examples...");
    populateExamples();
    logInfo("Updating UI...");
    updateUI();
    clearDiagnosticSummary();
    lastCompileTime = null;
    setupAutoCompile();
    setupUrlSharing();
    restoreFromUrl();
    currentState = "READY";
    logInfo("Playground initialization complete!");
  } catch (error) {
    logError("❌ Failed to initialize playground:", error);

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

  // Compile the example immediately
  await compileCode();
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

  // Start timing if not already set (for manual compile)
  if (!compileStartTime) {
    compileStartTime = performance.now();
  }
  const startTime = compileStartTime;

  try {
    logInfo("Beginning compilation process...");
    setStatus("loading", "Compiling...");

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

      // Set timing before updating diagnostic summary
      lastCompileTime = performance.now() - startTime;
      compileStartTime = null; // Reset for next compilation

      updateDiagnosticSummary();

      // Preserve current view instead of always showing diagnostics
      switch (currentView) {
        case "diagnostics":
          showDiagnostics();
          break;
        case "tokens":
          showTokens();
          break;
        case "parse":
          showParseAst();
          break;
        case "can":
          showCanCir();
          break;
        case "types":
          showTypes();
          break;
        default:
          showDiagnostics();
      }
    } else {
      logError("❌ Compilation failed:", response.message);
      lastCompileTime = performance.now() - startTime;
      compileStartTime = null; // Reset for next compilation
      setStatus("error", "Compilation failed");
      clearDiagnosticSummary();
      showError(`Compilation failed: ${response.message}`);
    }
  } catch (error) {
    logError("Error during compilation:", error);
    const errorMessage = error.message || error.toString() || "Unknown error";
    lastCompileTime = performance.now() - startTime;
    setStatus("error", "Compilation error");
    clearDiagnosticSummary();
    showError(`Error during compilation: ${errorMessage}`);
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
    showMessage("Compile code first to view PROBLEMS");
    return;
  }

  const outputContent = document.getElementById("outputContent");

  // Check if we have the new HTML format
  if (lastDiagnostics.html !== undefined) {
    // Use the HTML-rendered diagnostics directly
    if (lastDiagnostics.html.trim() === "") {
      outputContent.innerHTML =
        '<div class="success-message">✓ No issues found!</div>';
    } else {
      outputContent.innerHTML = lastDiagnostics.html;
    }
  } else {
    // Fallback to old format for compatibility
    let html = "";
    let totalErrors = 0;
    let totalWarnings = 0;

    // Count total diagnostics
    Object.values(lastDiagnostics).forEach((stageDiagnostics) => {
      stageDiagnostics.forEach((diagnostic) => {
        if (
          diagnostic.severity === "error" ||
          diagnostic.severity === "fatal"
        ) {
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
}

// Show tokens
async function showTokens() {
  currentView = "tokens";
  updateStageButtons();

  if (currentState !== "LOADED") {
    showMessage("Compile code first to view TOKENS");
    return;
  }

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
    logError("❌ Failed to query tokens:", error);
    showError(`Failed to query tokens: ${error.message}`);
  }
}

// Show parse AST
async function showParseAst() {
  currentView = "parse";
  updateStageButtons();

  if (currentState !== "LOADED") {
    showMessage("Compile code first to view AST");
    return;
  }

  try {
    const response = await sendMessage({
      type: "QUERY_AST",
    });
    if (response.status === "SUCCESS") {
      showSExpression(response.data);
    } else {
      showError(`Failed to get AST: ${response.message}`);
    }
  } catch (error) {
    logError("❌ Failed to query AST:", error);
    showError(`Failed to query AST: ${error.message}`);
  }
}

// Show CIR
async function showCanCir() {
  currentView = "can";
  updateStageButtons();

  if (currentState !== "LOADED") {
    showMessage("Compile code first to view CIR");
    return;
  }

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
    logError("❌ Failed to query CIR:", error);
    showError(`Failed to query CIR: ${error.message}`);
  }
}

// Show types
async function showTypes() {
  currentView = "types";
  updateStageButtons();

  if (currentState !== "LOADED") {
    showMessage("Compile code first to view TYPES");
    return;
  }

  try {
    const response = await sendMessage({
      type: "QUERY_TYPES",
    });
    if (response.status === "SUCCESS") {
      showSExpression(response.data);
    } else {
      showError(`Failed to get types: ${response.message}`);
    }
  } catch (error) {
    logError("❌ Failed to query types:", error);
    showError(`Failed to query types: ${error.message}`);
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
  const statusDot = document.getElementById("statusDot");
  const statusText = document.getElementById("statusText");

  if (statusDot) {
    statusDot.className = `status-dot ${status}`;
  }
  if (statusText) {
    statusText.textContent = text;
  }

  if (status === "loaded") {
    currentState = "LOADED";
  }
}

// Update UI based on current state
function updateUI() {
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


// Clear diagnostic summary from header
function clearDiagnosticSummary() {
  const editorHeader = document.querySelector(".editor-header");
  const existingSummary = editorHeader.querySelector(".diagnostic-summary");
  if (existingSummary) {
    existingSummary.remove();
  }
}

// Update diagnostic summary in header
function updateDiagnosticSummary() {
  const editorHeader = document.querySelector(".editor-header");

  // Remove existing diagnostic summary
  const existingSummary = editorHeader.querySelector(".diagnostic-summary");
  if (existingSummary) {
    existingSummary.remove();
  }

  if (!lastDiagnostics) {
    return;
  }

  let totalErrors = 0;
  let totalWarnings = 0;

  // Check if we have the new format with summary
  if (lastDiagnostics.summary) {
    totalErrors = lastDiagnostics.summary.errors;
    totalWarnings = lastDiagnostics.summary.warnings;
  } else {
    // Fallback to old format counting
    Object.values(lastDiagnostics).forEach((stageDiagnostics) => {
      if (Array.isArray(stageDiagnostics)) {
        stageDiagnostics.forEach((diagnostic) => {
          if (
            diagnostic.severity === "error" ||
            diagnostic.severity === "fatal"
          ) {
            totalErrors++;
          } else if (diagnostic.severity === "warning") {
            totalWarnings++;
          }
        });
      }
    });
  }

  // Always show summary after compilation (when timing info is available)
  if (lastCompileTime !== null) {
    const summaryDiv = document.createElement("div");
    summaryDiv.className = "diagnostic-summary";

    let summaryText = "";
    // Always show error/warning count after compilation
    summaryText += `Found ${totalErrors} error(s) and ${totalWarnings} warning(s)`;

    if (lastCompileTime !== null) {
      let timeText;
      if (lastCompileTime < 1000) {
        timeText = `${Math.round(lastCompileTime)}ms`;
      } else {
        timeText = `${(lastCompileTime / 1000).toFixed(1)}s`;
      }
      summaryText += (summaryText ? " • " : "") + `⚡ ${timeText}`;
    }

    summaryDiv.innerHTML = summaryText;
    editorHeader.appendChild(summaryDiv);
  }
}

// Auto-compile setup
let compileTimeout;
let compileStartTime = null;

function setupAutoCompile() {
  const editor = document.getElementById("editor");
  if (editor) {
    editor.addEventListener("input", () => {
      // Debounce compilation to avoid excessive calls
      clearTimeout(compileTimeout);
      compileStartTime = performance.now(); // Start timing when user stops typing
      compileTimeout = setTimeout(() => {
        if (currentState === "READY" || currentState === "LOADED") {
          compileCode();
        }
      }, 20); // 20ms delay for better responsiveness
    });
  }
}

// URL sharing functionality
function hashContent(content) {
  if (!content || content.length > 10000) return null; // Don't hash very large content

  let hash = 0;
  for (let i = 0; i < content.length; i++) {
    const char = content.charCodeAt(i);
    hash = (hash << 5) - hash + char;
    hash = hash & hash; // Convert to 32-bit integer
  }
  return Math.abs(hash).toString(36);
}

function updateUrl(content) {
  const hash = hashContent(content);
  if (hash) {
    const newUrl = `${window.location.origin}${window.location.pathname}?content=${hash}`;
    window.history.replaceState({}, "", newUrl);

    // Store content in localStorage with hash as key
    localStorage.setItem(`roc-content-${hash}`, content);
  } else {
    // Clear URL param if content is empty or too large
    const newUrl = `${window.location.origin}${window.location.pathname}`;
    window.history.replaceState({}, "", newUrl);
  }
}

function restoreFromUrl() {
  const urlParams = new URLSearchParams(window.location.search);
  const contentHash = urlParams.get("content");

  if (contentHash) {
    const savedContent = localStorage.getItem(`roc-content-${contentHash}`);
    if (savedContent) {
      const editor = document.getElementById("editor");
      if (editor) {
        editor.value = savedContent;
        logInfo("Restored content from URL hash:", contentHash);
      }
    }
  }
}

function setupUrlSharing() {
  const editor = document.getElementById("editor");
  if (editor) {
    editor.addEventListener("input", () => {
      // Debounce URL updates to avoid excessive calls
      clearTimeout(updateUrlTimeout);
      updateUrlTimeout = setTimeout(() => {
        updateUrl(editor.value);
      }, 1000); // Update URL after 1 second of no typing
    });
  }

  // Add share button to header
  addShareButton();
}

function addShareButton() {
  const headerStatus = document.querySelector(".header-status");
  if (headerStatus) {
    const shareButton = document.createElement("button");
    shareButton.className = "share-button";
    shareButton.innerHTML = "share link";
    shareButton.title = "Copy shareable link to clipboard";
    shareButton.onclick = copyShareLink;

    // Insert before the theme toggle
    const themeToggle = headerStatus.querySelector(".theme-toggle");
    headerStatus.insertBefore(shareButton, themeToggle);
  }
}

function copyShareLink() {
  const editor = document.getElementById("editor");
  if (editor) {
    const content = editor.value.trim();
    if (content) {
      const hash = hashContent(content);
      if (hash) {
        localStorage.setItem(`roc-content-${hash}`, content);
        const shareUrl = `${window.location.origin}${window.location.pathname}?content=${hash}`;

        navigator.clipboard
          .writeText(shareUrl)
          .then(() => {
            // Show temporary feedback
            const shareButton = document.querySelector(".share-button");
            const originalText = shareButton.innerHTML;
            shareButton.innerHTML = "copied";

            setTimeout(() => {
              shareButton.innerHTML = originalText;
            }, 2000);
          })
          .catch((err) => {
            console.error("Failed to copy to clipboard:", err);
            // Fallback: show the URL in an alert
            alert(`Share this link: ${shareUrl}`);
          });
      } else {
        alert("Content is too large to share via URL");
      }
    } else {
      alert("No content to share");
    }
  }
}

// Utility functions
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

  // Update theme label text
  updateThemeLabel(theme);
}

function toggleTheme() {
  const currentTheme = document.documentElement.getAttribute("data-theme");
  const newTheme = currentTheme === "dark" ? "light" : "dark";

  document.documentElement.setAttribute("data-theme", newTheme);
  localStorage.setItem("theme", newTheme);

  const themeSwitch = document.getElementById("themeSwitch");
  themeSwitch.setAttribute("aria-checked", newTheme === "dark");

  // Update theme label text
  updateThemeLabel(newTheme);
}

function updateThemeLabel(theme) {
  const themeLabel = document.querySelector(".theme-label");
  if (themeLabel) {
    themeLabel.textContent = theme === "dark" ? "Dark" : "Light";
  }
}

function logInfo(...args) {
  console.log("INFO:", ...args);
}

function logWarn(...args) {
  console.warn("WARNING:", ...args);
}

function logError(...args) {
  console.error("ERROR:", ...args);
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
