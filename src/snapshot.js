// Token highlighting functionality
document.addEventListener("DOMContentLoaded", function () {
  // Pane switching functionality
  window.switchLeftPane = function () {
    const selector = document.getElementById("left-selector");
    const selectedSection = selector.value;
    switchToPane("left", selectedSection);
    // Save to localStorage
    localStorage.setItem("leftPaneSelection", selectedSection);
  };

  window.switchRightPane = function () {
    const selector = document.getElementById("right-selector");
    const selectedSection = selector.value;
    switchToPane("right", selectedSection);
    // Save to localStorage
    localStorage.setItem("rightPaneSelection", selectedSection);
  };

  // Setup event delegation for source-range elements
  setupSourceRangeHandlers();

  function switchToPane(pane, sectionName) {
    const paneContent = document.getElementById(`${pane}-pane-content`);

    const paneSelector = document.getElementById(`${pane}-selector`);
    if (paneSelector.value !== sectionName) {
      paneSelector.value = sectionName;
    }

    // Handle special sections with JavaScript-generated content
    if (sectionName === "SOURCE") {
      // Initialize source display with byte range highlighting
      paneContent.innerHTML =
        '<div class="source-code" id="source-display"></div>';
      if (typeof initializeSourceDisplay === "function") {
        initializeSourceDisplay();
      }
      return;
    }

    if (sectionName === "TOKENS") {
      // Initialize tokens display with hover highlighting
      paneContent.innerHTML =
        '<div class="token-list" id="tokens-display"></div>';
      if (typeof initializeTokensDisplay === "function") {
        initializeTokensDisplay();
      }
      return;
    }

    // Find the section in the data sections for other sections
    const sections = document.querySelectorAll("#data-sections .section");
    let targetSectionContent = null;

    for (const section of sections) {
      if (section.dataset.section === sectionName) {
        const content = section.querySelector(".section-content");
        if (content) {
          targetSectionContent = content.cloneNode(true);
          break;
        }
      }
    }

    // Update pane content
    if (targetSectionContent && paneContent) {
      paneContent.innerHTML = "";
      paneContent.appendChild(targetSectionContent);
    }
  }

  // Initial setup - restore from localStorage or use defaults
  const savedLeftPane = localStorage.getItem("leftPaneSelection") || "SOURCE";
  const savedRightPane =
    localStorage.getItem("rightPaneSelection") || "CANONICALIZE";

  switchToPane("left", savedLeftPane);
  switchToPane("right", savedRightPane);
});
// UTF-8 byte handling functions from rust_highlight_demo.html
function getUtf8Bytes(str) {
  return new TextEncoder().encode(str);
}

function decodeUtf8AndBuildMapping(bytes) {
  const decoder = new TextDecoder("utf-8");
  const text = decoder.decode(bytes);
  const byteToUtf16 = new Array(bytes.length);
  const encoder = new TextEncoder();

  let byteIndex = 0;
  for (let i = 0; i < text.length; i++) {
    const char = text[i];
    const utf8 = encoder.encode(char);
    // Map all bytes of this character to the same UTF-16 position
    for (let j = 0; j < utf8.length; j++) {
      if (byteIndex < bytes.length) {
        byteToUtf16[byteIndex] = i;
        byteIndex++;
      }
    }
  }
  return { text, byteToUtf16 };
}

function buildTextClassSegments(text, byteToUtf16, tokens, highlightRange) {
  const segments = [];
  let cursor = 0;
  let tokenIdx = 0;
  const totalBytes = byteToUtf16.length;

  let hlStart = highlightRange ? highlightRange.start : null;
  let hlEnd = highlightRange
    ? highlightRange.start + highlightRange.length
    : null;

  while (cursor < totalBytes) {
    // Find next token that starts at or after cursor
    while (
      tokenIdx < tokens.length &&
      tokens[tokenIdx].start + tokens[tokenIdx].length <= cursor
    ) {
      tokenIdx++;
    }

    let nextToken = tokenIdx < tokens.length ? tokens[tokenIdx] : null;
    let tokenStart = nextToken ? Math.max(nextToken.start, cursor) : totalBytes;
    let tokenEnd = nextToken ? nextToken.start + nextToken.length : totalBytes;

    let boundaries = [tokenStart, tokenEnd];
    if (hlStart !== null) boundaries.push(hlStart);
    if (hlEnd !== null) boundaries.push(hlEnd);
    boundaries.push(totalBytes);

    let nextBoundary = Math.min(...boundaries.filter((b) => b > cursor));
    if (nextBoundary === Infinity) nextBoundary = totalBytes;

    let inToken = nextToken && cursor >= nextToken.start && cursor < tokenEnd;
    let tokenClass = inToken ? nextToken.class : null;
    let inHighlight = hlStart !== null && cursor >= hlStart && cursor < hlEnd;

    let classes = [];
    if (tokenClass) classes.push(tokenClass);
    if (inHighlight) classes.push("highlight");
    let classStr = classes.length ? classes.join(" ") : null;

    let start16 = byteToUtf16[cursor] ?? text.length;
    let end16 =
      byteToUtf16[Math.min(nextBoundary, totalBytes - 1)] ?? text.length;
    if (nextBoundary >= totalBytes) end16 = text.length;

    if (start16 < end16) {
      const segment = { text: text.slice(start16, end16), class: classStr };
      // Store byte range instead of token index for consistent mapping
      if (inToken) {
        segment.byteStart = nextToken.start;
        segment.byteEnd = nextToken.start + nextToken.length;
      }
      segments.push(segment);
    }

    cursor = nextBoundary;

    // If we reached the end of a token, potentially advance tokenIdx
    if (nextToken && cursor >= tokenEnd) {
      tokenIdx++;
    }
  }

  return segments;
}

function updateDomFromSegments(container, newSegments) {
  container.innerHTML = "";
  for (const seg of newSegments) {
    if (seg.class) {
      const span = document.createElement("span");
      span.className = seg.class;
      span.textContent = seg.text;
      // Add byte range data attributes for consistent mapping
      if (seg.byteStart !== undefined && seg.byteEnd !== undefined) {
        span.dataset.byteStart = seg.byteStart;
        span.dataset.byteEnd = seg.byteEnd;
        span.style.cursor = "pointer";
      }
      container.appendChild(span);
    } else {
      container.appendChild(document.createTextNode(seg.text));
    }
  }
  // Add event listeners for reverse highlighting
  addSourceEventListeners(container);
}

// Token category mapping
function getTokenClass(tokenKind) {
  const keywords = [
    "KwApp",
    "KwAs",
    "KwCrash",
    "KwDbg",
    "KwElse",
    "KwExpect",
    "KwExposes",
    "KwExposing",
    "KwFor",
    "KwGenerates",
    "KwHas",
    "KwHosted",
    "KwIf",
    "KwImplements",
    "KwImport",
    "KwImports",
    "KwIn",
    "KwInterface",
    "KwMatch",
    "KwModule",
    "KwPackage",
    "KwPackages",
    "KwPlatform",
    "KwProvides",
    "KwRequires",
    "KwReturn",
    "KwVar",
    "KwWhere",
    "KwWith",
  ];
  const identifiers = [
    "UpperIdent",
    "LowerIdent",
    "DotLowerIdent",
    "DotUpperIdent",
    "NoSpaceDotLowerIdent",
    "NoSpaceDotUpperIdent",
    "NamedUnderscore",
    "OpaqueName",
  ];
  const strings = [
    "StringStart",
    "StringEnd",
    "StringPart",
    "MultilineStringStart",
    "MultilineStringEnd",
    "SingleQuote",
  ];
  const numbers = [
    "Float",
    "Int",
    "DotInt",
    "NoSpaceDotInt",
    "MalformedNumberBadSuffix",
    "MalformedNumberUnicodeSuffix",
    "MalformedNumberNoDigits",
    "MalformedNumberNoExponentDigits",
  ];
  const operators = [
    "OpPlus",
    "OpStar",
    "OpBinaryMinus",
    "OpUnaryMinus",
    "OpEquals",
    "OpNotEquals",
    "OpAnd",
    "OpOr",
    "OpGreaterThan",
    "OpLessThan",
    "OpGreaterThanOrEq",
    "OpLessThanOrEq",
    "OpAssign",
    "OpColonEqual",
    "OpArrow",
    "OpBackslash",
    "OpBar",
    "OpBang",
    "OpQuestion",
    "OpColon",
    "OpPercent",
    "OpDoubleSlash",
    "OpCaret",
    "OpAmpersand",
    "OpPizza",
    "OpSlash",
    "OpDoubleQuestion",
    "OpBackArrow",
    "OpFatArrow",
    "NoSpaceOpQuestion",
  ];
  const brackets = [
    "OpenRound",
    "CloseRound",
    "OpenSquare",
    "CloseSquare",
    "OpenCurly",
    "CloseCurly",
  ];
  const punctuation = ["Comma", "Dot", "DoubleDot", "TripleDot", "Underscore"];

  if (keywords.includes(tokenKind)) return "token-keyword";
  if (identifiers.includes(tokenKind)) return "token-identifier";
  if (strings.includes(tokenKind)) return "token-string";
  if (numbers.includes(tokenKind)) return "token-number";
  if (operators.includes(tokenKind)) return "token-operator";
  if (brackets.includes(tokenKind)) return "token-bracket";
  if (punctuation.includes(tokenKind)) return "token-punctuation";
  return "token-default";
}

function isSectionMarkup(currentSection) {
  return (
    currentSection === "TOKENS" ||
    currentSection === "PARSE" ||
    currentSection === "CANONICALIZE" ||
    currentSection === "TYPES"
  );
}

// Add event listeners for source elements to highlight corresponding tokens or PARSE elements
function addSourceEventListeners(container) {
  const spans = container.querySelectorAll(
    "span[data-byte-start][data-byte-end]",
  );
  spans.forEach((span) => {
    span.addEventListener("mouseenter", () => {
      const byteStart = parseInt(span.dataset.byteStart);
      const byteEnd = parseInt(span.dataset.byteEnd);

      // Check what's currently displayed in the right pane
      const rightSelector = document.getElementById("right-selector");
      const currentSection = rightSelector ? rightSelector.value : "TOKENS";

      if (isSectionMarkup(currentSection)) {
        highlightMarkupElementAtByte(byteStart);
      }
    });
    span.addEventListener("mouseleave", () => {
      const rightSelector = document.getElementById("right-selector");
      const currentSection = rightSelector ? rightSelector.value : "TOKENS";

      if (isSectionMarkup(currentSection)) {
        clearMarkupHighlights();
      }
    });
    span.addEventListener("click", (event) => {
      const byteStart = parseInt(span.dataset.byteStart);
      const byteEnd = parseInt(span.dataset.byteEnd);

      // Get the click position within the span to determine the exact byte
      const clickByte = getClickBytePosition(event, span, byteStart, byteEnd);

      const rightSelector = document.getElementById("right-selector");
      const currentSection = rightSelector ? rightSelector.value : "TOKENS";

      if (isSectionMarkup(currentSection)) {
        scrollToMarkupElementAtByte(clickByte);
      }
    });
  });
}

function scrollToAndFlash(element, container) {
  // Calculate if element is outside the visible area
  const containerRect = container.getBoundingClientRect();
  const elementRect = element.getBoundingClientRect();

  const isAbove = elementRect.top < containerRect.top;
  const isBelow = elementRect.bottom > containerRect.bottom;

  if (isAbove || isBelow) {
    // Calculate the scroll position to center the element
    const containerScrollTop = container.scrollTop;
    const elementOffsetTop = element.offsetTop;
    const containerHeight = container.clientHeight;
    const elementHeight = element.offsetHeight;

    const scrollTo = elementOffsetTop - containerHeight / 2 + elementHeight / 2;

    // Smooth scroll to the target position
    container.scrollTo({
      top: scrollTo,
      behavior: "smooth",
    });
  }

  // Add flash animation
  element.classList.remove("flash-highlight");
  // Force reflow to restart animation
  element.offsetHeight;
  element.classList.add("flash-highlight");

  // Remove the flash class after animation completes
  setTimeout(() => {
    element.classList.remove("flash-highlight");
  }, 600);
}

// Initialize display when sections are selected
function initializeSourceDisplay() {
  if (!window.rocSourceCode) return;
  const sourceElem = document.getElementById("source-display");
  if (sourceElem) {
    const bytes = getUtf8Bytes(window.rocSourceCode);
    const { text, byteToUtf16 } = decodeUtf8AndBuildMapping(bytes);
    window.rocSourceMapping = { bytes, text, byteToUtf16 };

    // Convert tokens to format expected by buildTextClassSegments
    if (window.rocTokens) {
      window.rocTokensFormatted = window.rocTokens
        .map(([kind, start, end]) => ({
          start,
          length: end - start,
          class: getTokenClass(kind),
        }))
        .sort((a, b) => a.start - b.start);
    }

    renderSourceHighlight(null);
  }
}

function initializeTokensDisplay() {
  if (!window.rocTokens) return;
  const tokensElem = document.getElementById("tokens-display");
  if (tokensElem) {
    tokensElem.innerHTML = "";
    window.rocTokens.forEach(([kind, start, end], index) => {
      const div = document.createElement("div");
      div.className = "token-item " + getTokenClass(kind);
      div.textContent = `${kind}`;
      // Store both index and byte ranges for consistent mapping
      div.dataset.tokenIndex = index;
      div.dataset.byteStart = start;
      div.dataset.byteEnd = end;
      div.style.cursor = "pointer";

      // Make the token element compatible with markup highlighting system
      div.classList.add("source-range");
      div.dataset.startByte = start;
      div.dataset.endByte = end;

      tokensElem.appendChild(div);
    });
  }
}

function renderSourceHighlight(range) {
  if (!window.rocSourceMapping) return;
  const sourceElem = document.getElementById("source-display");
  if (!sourceElem) return;

  const { text, byteToUtf16 } = window.rocSourceMapping;
  const tokens = window.rocTokensFormatted || [];
  const segments = buildTextClassSegments(text, byteToUtf16, tokens, range);
  updateDomFromSegments(sourceElem, segments);
}

// Setup event delegation for source-range elements in PARSE tree and other sections
function setupSourceRangeHandlers() {
  document.addEventListener(
    "mouseenter",
    function (event) {
      if (event.target.classList.contains("source-range")) {
        const startByte = parseInt(event.target.dataset.startByte);
        const endByte = parseInt(event.target.dataset.endByte);
        if (!isNaN(startByte) && !isNaN(endByte)) {
          highlightSourceRange(startByte, endByte);
        }
      }
    },
    true,
  );

  document.addEventListener(
    "mouseleave",
    function (event) {
      if (event.target.classList.contains("source-range")) {
        clearSourceRangeHighlight();
      }
    },
    true,
  );

  document.addEventListener(
    "click",
    function (event) {
      if (event.target.classList.contains("source-range")) {
        const startByte = parseInt(event.target.dataset.startByte);
        const endByte = parseInt(event.target.dataset.endByte);
        if (!isNaN(startByte) && !isNaN(endByte)) {
          scrollToSourceRange(startByte, endByte);
        }
      }
    },
    true,
  );
}

// Highlight source range in the SOURCE pane
function highlightSourceRange(startByte, endByte) {
  const range = { start: startByte, length: endByte - startByte };
  renderSourceHighlight(range);
}

// Clear source range highlighting
function clearSourceRangeHighlight() {
  renderSourceHighlight(null);
}

// Scroll to and highlight a source range using yellow highlighting
function scrollToSourceRange(startByte, endByte) {
  const sourceContainer = document.getElementById("source-display");
  if (!sourceContainer) return;

  // Always use byte range highlighting with synthetic elements (yellow background)
  highlightSourceRange(startByte, endByte);

  // Find the best element to scroll to by looking for overlapping spans
  const allSpans = sourceContainer.querySelectorAll(
    "span[data-byte-start][data-byte-end]",
  );
  let targetSpan = null;

  for (const span of allSpans) {
    const spanStart = parseInt(span.dataset.byteStart);
    const spanEnd = parseInt(span.dataset.byteEnd);

    // Check if this span overlaps with our target range
    if (spanStart <= startByte && startByte < spanEnd) {
      targetSpan = span;
      break;
    }
  }

  // If no overlapping token span found, look for the highlighted elements themselves
  if (!targetSpan) {
    const highlightedSpans = sourceContainer.querySelectorAll("span.highlight");
    if (highlightedSpans.length > 0) {
      targetSpan = highlightedSpans[0]; // Use the first highlighted element
    }
  }

  if (targetSpan) {
    const paneContent = sourceContainer.closest(".pane-content");
    if (paneContent) {
      // Scroll to the target span
      const containerRect = paneContent.getBoundingClientRect();
      const elementRect = targetSpan.getBoundingClientRect();

      const isAbove = elementRect.top < containerRect.top;
      const isBelow = elementRect.bottom > containerRect.bottom;

      if (isAbove || isBelow) {
        const containerScrollTop = paneContent.scrollTop;
        const elementOffsetTop = targetSpan.offsetTop;
        const containerHeight = paneContent.clientHeight;
        const elementHeight = targetSpan.offsetHeight;

        const scrollTo =
          elementOffsetTop - containerHeight / 2 + elementHeight / 2;

        paneContent.scrollTo({
          top: scrollTo,
          behavior: "smooth",
        });
      }
    }
  }

  // Add flashing animation to the highlighted synthetic elements
  flashHighlightedElements();
}

// Flash the currently highlighted synthetic elements
function flashHighlightedElements() {
  const sourceContainer = document.getElementById("source-display");
  if (!sourceContainer) return;

  const highlightedSpans = sourceContainer.querySelectorAll("span.highlight");
  highlightedSpans.forEach((span) => {
    span.classList.add("flash-highlight");
  });

  // Remove the flash class after animation completes
  setTimeout(() => {
    highlightedSpans.forEach((span) => {
      span.classList.remove("flash-highlight");
    });
  }, 600);
}

// Get the exact byte position within a span based on click coordinates
function getClickBytePosition(event, span, byteStart, byteEnd) {
  const spanRect = span.getBoundingClientRect();
  const clickX = event.clientX - spanRect.left;
  const spanWidth = spanRect.width;

  if (spanWidth === 0) return byteStart;

  // Calculate the relative position within the span (0 to 1)
  const relativePosition = Math.max(0, Math.min(1, clickX / spanWidth));

  // Map to byte position
  const byteRange = byteEnd - byteStart;
  const clickByte = Math.floor(byteStart + relativePosition * byteRange);

  return Math.max(byteStart, Math.min(byteEnd - 1, clickByte));
}

// Find the narrowest markup element containing a specific byte position
function highlightMarkupElementAtByte(bytePosition) {
  const parseContainer = document.getElementById("right-pane-content");
  if (!parseContainer) return;

  const targetElement = findNarrowestMarkupElement(
    parseContainer,
    bytePosition,
  );
  if (targetElement) {
    // Clear existing highlights
    clearMarkupHighlights();

    // Highlight the target element
    targetElement.classList.add("highlighted");
  }
}

// Scroll to and flash the narrowest markup element containing a specific byte position
function scrollToMarkupElementAtByte(bytePosition) {
  const parseContainer = document.getElementById("right-pane-content");
  if (!parseContainer) return;

  const targetElement = findNarrowestMarkupElement(
    parseContainer,
    bytePosition,
  );
  if (targetElement) {
    // Clear existing highlights
    clearMarkupHighlights();

    // Highlight the target element
    targetElement.classList.add("highlighted");

    // Scroll to the element and flash it
    const paneContent = parseContainer.closest(".pane-content");
    if (paneContent) {
      scrollToAndFlash(targetElement, paneContent);
    }
  }
}

// Find the narrowest (most specific) element containing the byte position
function findNarrowestMarkupElement(container, bytePosition) {
  const sourceRangeElements = container.querySelectorAll(
    ".source-range[data-start-byte][data-end-byte]",
  );

  let narrowestElement = null;
  let narrowestRange = Infinity;

  for (const element of sourceRangeElements) {
    const startByte = parseInt(element.dataset.startByte);
    const endByte = parseInt(element.dataset.endByte);

    // Check if the byte position is within this element's range
    if (bytePosition >= startByte && bytePosition < endByte) {
      const range = endByte - startByte;

      // Choose this element if it's narrower than the current best
      // In case of ties, the first one encountered wins (DOM order)
      if (range < narrowestRange) {
        narrowestElement = element;
        narrowestRange = range;
      }
    }
  }

  return narrowestElement;
}

// Clear all highlights in the markup tree
function clearMarkupHighlights() {
  const parseContainer = document.getElementById("right-pane-content");
  if (parseContainer) {
    const highlightedElements = parseContainer.querySelectorAll(".highlighted");
    highlightedElements.forEach((element) => {
      element.classList.remove("highlighted");
    });
  }
}
