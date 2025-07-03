// Token highlighting functionality
document.addEventListener("DOMContentLoaded", function () {
  // Pane switching functionality
  window.switchLeftPane = function () {
    const selector = document.getElementById("left-selector");
    const selectedSection = selector.value;
    switchToPane("left", selectedSection);
  };

  window.switchRightPane = function () {
    const selector = document.getElementById("right-selector");
    const selectedSection = selector.value;
    switchToPane("right", selectedSection);
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

  // Initial setup - show SOURCE on left, TOKENS on right
  switchToPane("left", "SOURCE");
  switchToPane("right", "TOKENS");
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

// Central function to find token index by byte range - single source of truth
function findTokenIndexByByteRange(byteStart, byteEnd) {
  if (!window.rocTokens) return -1;
  return window.rocTokens.findIndex(
    ([kind, start, end]) => start === byteStart && end === byteEnd,
  );
}

// Central function to find token by index - single source of truth
function getTokenByIndex(index) {
  if (!window.rocTokens || index < 0 || index >= window.rocTokens.length)
    return null;
  const [kind, start, end] = window.rocTokens[index];
  return { kind, start, end, index };
}

// Add event listeners for source elements to highlight corresponding tokens
function addSourceEventListeners(container) {
  const spans = container.querySelectorAll(
    "span[data-byte-start][data-byte-end]",
  );
  spans.forEach((span) => {
    span.addEventListener("mouseenter", () => {
      const byteStart = parseInt(span.dataset.byteStart);
      const byteEnd = parseInt(span.dataset.byteEnd);
      const tokenIndex = findTokenIndexByByteRange(byteStart, byteEnd);
      if (tokenIndex >= 0) {
        highlightTokenInList(tokenIndex);
      }
    });
    span.addEventListener("mouseleave", () => {
      clearTokenHighlights();
    });
    span.addEventListener("click", () => {
      const byteStart = parseInt(span.dataset.byteStart);
      const byteEnd = parseInt(span.dataset.byteEnd);
      const tokenIndex = findTokenIndexByByteRange(byteStart, byteEnd);
      if (tokenIndex >= 0) {
        scrollToTokenInList(tokenIndex);
      }
    });
  });
}

function highlightTokenInList(tokenIndex) {
  const tokensContainer = document.getElementById("tokens-display");
  if (tokensContainer) {
    const tokenDivs = tokensContainer.querySelectorAll(".token-item");
    if (tokenDivs[tokenIndex]) {
      tokenDivs[tokenIndex].style.backgroundColor = "#ffffcc";
      tokenDivs[tokenIndex].style.outline = "2px solid #ffd700";
    }
  }
}

function clearTokenHighlights() {
  const tokensContainer = document.getElementById("tokens-display");
  if (tokensContainer) {
    const tokenDivs = tokensContainer.querySelectorAll(".token-item");
    tokenDivs.forEach((div) => {
      div.style.backgroundColor = "";
      div.style.outline = "";
    });
  }
}

function scrollToTokenInList(tokenIndex) {
  const tokensContainer = document.getElementById("tokens-display");
  if (tokensContainer) {
    const tokenDivs = tokensContainer.querySelectorAll(".token-item");
    if (tokenDivs[tokenIndex]) {
      const targetToken = tokenDivs[tokenIndex];
      const paneContent = tokensContainer.closest(".pane-content");
      if (paneContent) {
        scrollToAndFlash(targetToken, paneContent);
      }
    }
  }
}

function scrollToSourceToken(tokenIndex) {
  const token = getTokenByIndex(tokenIndex);
  if (!token) return;

  // Use byte range highlighting instead of flashing
  scrollToSourceRange(token.start, token.end);
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
      div.addEventListener("mouseenter", () => {
        renderSourceHighlight({ start, length: end - start });
      });
      div.addEventListener("mouseleave", () => {
        renderSourceHighlight(null);
      });
      div.addEventListener("click", () => {
        scrollToSourceToken(index);
      });
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
  const allSpans = sourceContainer.querySelectorAll("span[data-byte-start][data-byte-end]");
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
        
        const scrollTo = elementOffsetTop - containerHeight / 2 + elementHeight / 2;
        
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
  highlightedSpans.forEach(span => {
    span.classList.add("flash-highlight");
  });

  // Remove the flash class after animation completes
  setTimeout(() => {
    highlightedSpans.forEach(span => {
      span.classList.remove("flash-highlight");
    });
  }, 600);
}
