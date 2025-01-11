const isOnMobile = window.innerWidth <= 1024;

// The only way we can provide values to wasm_bindgen's generated code is to set globals
window.js_create_app = js_create_app;
window.js_run_app = js_run_app;
window.js_get_result_and_memory = js_get_result_and_memory;

// The only place we use console.error is in wasm_bindgen, where it gets a single string argument.
console.error = function displayErrorInHistoryPanel(string) {
  const html = `<div class="panic">${string}</div>`;
  updateHistoryEntry(repl.inputHistoryIndex, false, html);
};

import * as roc_repl_wasm from "./repl/roc_repl_wasm.js";

const isHomepage = document.getElementById("homepage-repl-container") != null;

const tutorialButtonSvg = `<svg viewBox="0 -6 51 58" xmlns="http://www.w3.org/2000/svg" aria-labelledby="repl-tutorial-link" role="img" class="roc-logo"><title id="repl-tutorial-link">Return to Roc Home</title><polygon role="presentation" points="0,0 23.8834,3.21052 37.2438,19.0101 45.9665,16.6324 50.5,22 45,22 44.0315,26.3689 26.4673,39.3424 27.4527,45.2132 17.655,53 23.6751,22.7086"></polygon></svg>`;

// ----------------------------------------------------------------------------
// REPL state
// ----------------------------------------------------------------------------

const repl = {
  elemHistory: document.getElementById("history-text"),
  elemSourceInput: document.getElementById("source-input"),
  description: document.getElementById("repl-description"),

  inputQueue: [],
  inputHistory: [],
  inputHistoryIndex: 0,
  inputStash: "", // stash the user input while we're toggling through history with up/down arrows

  // Current progress through the repl tutorial
  tutorialStep: 0,
  tutorialSteps: [
    {
      match: (input) => input.replace(/ /g, "") === "0.1+0.2",
      show: '<p>Was this the answer you expected? (If so, try this in other programming languages and see what their answers are.)</p><p>Roc has a <a href="/builtins/Num#Dec">decimal</a> type as well as <a href="/builtins/Num#F64">floating-point</a> for when performance is more important than decimal precision.</p><p>Next, enter <code>name = "(put your name here)"</code></p>',
    },
    {
      match: (input) => input.replace(/ /g, "").match(/^name="/i),
      show: '<p>This created a new <a href="https://www.roc-lang.org/tutorial#defs">definition</a>&mdash;<code>name</code> is now defined to be equal to the <a href="/tutorial#strings-and-numbers">string</a> you entered.</p><p>Try using this definition by entering <code>"Hi, \${name}!"</code></p>',
    },
    {
      match: (input) => input.match(/^"[^\$]+\$\(name\)/i),
      show: `<p>Nicely done! This is an example of <a href=\"/tutorial#string-interpolation\">string interpolation</a>, which replaces part of a string with whatever you put inside the parentheses after a <code>$</code>.</p><p>Now that you’ve written a few <a href=\"/tutorial#naming-things\">expressions</a>, you can either continue exploring in this REPL, or move on to the <a href=\"/tutorial\">tutorial</a> to learn how to make full programs.<p><p><span class='welcome-to-roc'>Welcome to Roc!</span> <a href='/tutorial' class='btn-small'>${tutorialButtonSvg} Start Tutorial</a></p>`,
    },
  ],

  textDecoder: new TextDecoder(),
  textEncoder: new TextEncoder(),

  compiler: null,
  app: null,

  // Temporary storage for the address of the result of running the user's code.
  // Used while control flow returns to Rust to allocate space to copy the app's memory buffer.
  result_addr: 0,
};

// Initialise
repl.elemSourceInput.value = ""; // Some browsers remember the input across refreshes
resetSourceInputHeight();
repl.elemSourceInput.addEventListener("input", resetSourceInputHeight);
repl.elemSourceInput.addEventListener("keydown", onInputKeydown);
repl.elemSourceInput.addEventListener("keyup", onInputKeyup);
roc_repl_wasm.default("/repl/roc_repl_wasm_bg.wasm").then(async (instance) => {
  const loadingMessage = repl.elemHistory.querySelector("#loading-message");

  if (loadingMessage != null) {
    loadingMessage.remove();
  }

  repl.elemSourceInput.placeholder = "Enter some Roc code here.";
  repl.compiler = instance;

  // Get help text from the compiler, and display it at top of the history panel
  try {
    const helpText = await roc_repl_wasm.entrypoint_from_js(":help");
    const helpElem = document.getElementById("help-text");

    if (helpElem != null) {
      helpElem.innerHTML = helpText.trim();
    }
  } catch (e) {
    // Print error for Roc devs. Don't use console.error, we overrode that above to display on the page!
    console.warn(e);
  }
});

// Focus the repl input the first time it scrolls into view
// (but not on mobile, because that would pop up the whole keyboard abruptly)
if (!isOnMobile) {
  // Function to be called when the input enters the viewport
  function handleIntersect(entries, observer) {
    entries.forEach((entry) => {
      // Check if the input is intersecting
      if (entry.isIntersecting) {
        // Apply focus to it, then unobserve it because we only want to do this once.
        entry.target.focus();
        observer.unobserve(entry.target);
      }
    });
  }

  // Set up the Intersection Observer
  let observer = new IntersectionObserver(handleIntersect, {
    // Use the whole viewport for the intersection
    root: null,
    // Trigger the callback when the input is fully visible
    threshold: 1.0,
  });

  observer.observe(repl.elemSourceInput);
}

// ----------------------------------------------------------------------------
// Handle inputs
// ----------------------------------------------------------------------------

function resetSourceInputHeight() {
  repl.elemSourceInput.style.height =
    repl.elemSourceInput.scrollHeight + 2 + "px"; // +2 for the border
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

      // Hide the arrow on the homepage that prompts you to enter something
      const replArrow = document.getElementById("repl-arrow");

      if (replArrow != null) {
        replArrow.style.display = "none";
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

function showNextReplTutorialEntry(inputText) {
  const nextStep = repl.tutorialSteps[repl.tutorialStep];

  if (repl.description != null && typeof nextStep === "object" && nextStep.match(inputText)) {
    repl.description.innerHTML =
      repl.description.innerHTML + "<hr/>" + nextStep.show;

    repl.tutorialStep = repl.tutorialStep + 1;
  }
}

// Use a queue just in case we somehow get inputs very fast
// We want the REPL to only process one at a time, since we're using some global state.
// In normal usage we shouldn't see this edge case anyway. Maybe with copy/paste?
async function processInputQueue() {
  while (repl.inputQueue.length) {
    const inputText = repl.inputQueue[0];

    if (inputText) {
      repl.inputHistoryIndex = createHistoryEntry(inputText);
      repl.inputStash = "";

      let outputText = "";
      let ok = true;
      try {
        outputText = await roc_repl_wasm.entrypoint_from_js(inputText);
      } catch (e) {
        outputText = `${e}`;
        ok = false;
      }

      updateHistoryEntry(repl.inputHistoryIndex, ok, outputText);
      showNextReplTutorialEntry(inputText);
    }

    repl.inputQueue.shift();
  }
}

// ----------------------------------------------------------------------------
// Callbacks to JS from Rust
// ----------------------------------------------------------------------------

var ROC_PANIC_INFO = null;

function send_panic_msg_to_js(rocstr_ptr, panic_tag) {
  const { memory } = repl.app.exports;

  const rocStrBytes = new Int8Array(memory.buffer, rocstr_ptr, 12);
  const finalByte = rocStrBytes[11];

  let stringBytes = "";
  if (finalByte < 0) {
    // small string

    // bitwise ops on negative JS numbers are weird. This clears the bit that we
    // use to indicate a small string. In rust it's `finalByte as u8 ^ 0b1000_0000`
    const length = finalByte + 128;
    stringBytes = new Uint8Array(memory.buffer, rocstr_ptr, length);
  } else {
    // big string
    const rocStrWords = new Uint32Array(memory.buffer, rocstr_ptr, 3);
    const [ptr, len, _cap] = rocStrWords;

    const SEAMLESS_SLICE_BIT = 1 << 31;
    const length = len & ~SEAMLESS_SLICE_BIT;

    stringBytes = new Uint8Array(memory.buffer, ptr, length);
  }

  const decodedString = repl.textDecoder.decode(stringBytes);

  ROC_PANIC_INFO = {
    msg: decodedString,
    panic_tag: panic_tag,
  };
}

// Load Wasm code into the browser's virtual machine, so we can run it later.
// This operation is async, so we call it before entering any code shared
// with the command-line REPL, which is sync.
async function js_create_app(wasm_module_bytes) {
  const { instance } = await WebAssembly.instantiate(wasm_module_bytes, {
    env: {
      send_panic_msg_to_js: send_panic_msg_to_js,
    },
  });

  // Keep the instance alive so we can run it later from shared REPL code
  repl.app = instance;
}

// Call the `main` function of the user app, via the `wrapper` function.
function js_run_app() {
  const { wrapper, memory } = repl.app.exports;

  // Run the user code, and remember the result address
  // We'll pass it to Rust in the next callback
  try {
    repl.result_addr = wrapper();
  } catch (e) {
    // an exception could be that roc_panic was invoked,
    // or some other crash (likely a compiler bug)
    if (ROC_PANIC_INFO === null) {
      throw e;
    } else {
      // when roc_panic set an error message, display it
      const { msg, panic_tag } = ROC_PANIC_INFO;
      ROC_PANIC_INFO = null;

      console.error(format_roc_panic_message(msg, panic_tag));
    }
  }

  // Tell Rust how much space to reserve for its copy of the app's memory buffer.
  // We couldn't know that size until we actually ran the app.
  return memory.buffer.byteLength;
}

function format_roc_panic_message(msg, panic_tag) {
  switch (panic_tag) {
    case 0: {
      return `Roc failed with message: "${msg}"`;
    }
    case 1: {
      return `User crash with message: "${msg}"`;
    }
    default: {
      return `Got an invalid panic tag: "${panic_tag}"`;
    }
  }
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

  if (isHomepage) {
    // Scroll the input element into view so you can see the most recent output.
    // Only do this if it's currently out of view though!
    const bounds = repl.elemSourceInput.getBoundingClientRect();
    const isInView =
      bounds.top >= 0 &&
      bounds.left >= 0 &&
      bounds.bottom <=
      (window.innerHeight || document.documentElement.clientHeight) &&
      bounds.right <=
      (window.innerWidth || document.documentElement.clientWidth);

    if (!isInView) {
      repl.elemSourceInput.scrollIntoView({
        behavior: "instant",
        block: "end",
        inline: "nearest",
      });
    }
  } else {
    // Scroll the page to the bottom so you can see the most recent output.
    window.scrollTo(0, document.body.scrollHeight);
  }
}

// TUTORIAL //

const tutorialTocToggle = document.querySelector("#tutorial-toc-toggle");

if (tutorialTocToggle != null) {
  document.querySelectorAll("#tutorial-toc li a").forEach((elem) => {
    // Clicking any of the ToC links closes the ToC
    elem.addEventListener("click", (event) => {
      tutorialTocToggle.checked = false;
    });
  });

  document.addEventListener("keydown", (event) => {
    // Escape closes the ToC
    if (event.key == "Escape") {
      tutorialTocToggle.checked = false;
    }
  });

  const isTouchSupported = () => {
    try {
      document.createEvent("TouchEvent");
      return true;
    } catch (e) {
      return false;
    }
  };

  // Select all <samp> elements that are children of <pre> elements
  const codeBlocks = document.querySelectorAll("pre > samp");

  // Iterate over each code block
  codeBlocks.forEach((codeBlock) => {
    // Create a "Copy" button
    const copyButton = document.createElement("button");
    copyButton.classList.add("copy-button");
    copyButton.textContent = "Copy";

    // Add event listener to copy button
    copyButton.addEventListener("click", () => {
      const codeText = codeBlock.innerText;
      navigator.clipboard.writeText(codeText);
      copyButton.textContent = "Copied!";
      copyButton.classList.add("copy-button-copied");
      copyButton.addEventListener("mouseleave", () => {
        copyButton.textContent = "Copy";
        copyButton.classList.remove("copy-button-copied");
      });
    });

    // Create a container for the copy button and append it to the document
    const buttonContainer = document.createElement("div");
    buttonContainer.classList.add("button-container");
    buttonContainer.appendChild(copyButton);
    codeBlock.parentNode.insertBefore(buttonContainer, codeBlock);

    // Hide the button container by default
    buttonContainer.style.display = "none";

    if (isTouchSupported()) {
      // Show the button container on click for touch support (e.g. mobile)
      document.addEventListener("click", (event) => {
        if (event.target.closest("pre > samp") !== codeBlock) {
          buttonContainer.style.display = "none";
        } else {
          buttonContainer.style.display = "block";
        }
      });
    } else {
      // Show the button container on hover for non-touch support (e.g. desktop)
      codeBlock.parentNode.addEventListener("mouseenter", () => {
        buttonContainer.style.display = "block";
      });

      codeBlock.parentNode.addEventListener("mouseleave", () => {
        buttonContainer.style.display = "none";
      });
    }
  });
}

// HOMEPAGE //

if (isOnMobile) {
  const hideDesc = () => {
    document.querySelectorAll(".interactive-radio").forEach((radio) => {
      radio.checked = false;
    });
  };

  hideDesc(); // On mobile, start out with all the descriptions hidden.

  document.querySelectorAll(".interactive-example").forEach((example) => {
    example.querySelectorAll("label").forEach((label) => {
      label.addEventListener("click", (event) => {
        const desc = label.nextSibling; // The description node always comes next

        // Set the position of the target element
        desc.style.top = label.offsetTop + label.offsetHeight + "px"; // Position below the button
        desc.style.left = label.offsetLeft + "px"; // Align with the left of the button
      });
    });

    example.querySelectorAll(".close-desc").forEach((button) => {
      button.style.display = "block";
      button.addEventListener("click", hideDesc);
    });
  });
}
