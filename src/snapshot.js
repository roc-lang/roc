// Token highlighting functionality
document.addEventListener("DOMContentLoaded", function () {
  // Use event delegation for token highlighting to avoid re-adding listeners
  function setupTokenHighlighting() {
    // Remove any existing event listeners
    document.removeEventListener("mouseenter", tokenHighlightHandler, true);
    document.removeEventListener("mouseleave", tokenUnhighlightHandler, true);
    document.removeEventListener("click", tokenClickHandler, true);

    // Add event listeners using delegation
    document.addEventListener("mouseenter", tokenHighlightHandler, true);
    document.addEventListener("mouseleave", tokenUnhighlightHandler, true);
    document.addEventListener("click", tokenClickHandler, true);
  }

  function highlightTokens(min, max) {
    for (let i = min; i <= max; i++) {
      const token = document.querySelector(`[data-token-id="${i}"]`);
      if (token) {
        token.classList.add("highlighted");
      }
    }
  }

  function unhighlightTokens(min, max) {
    for (let i = min; i <= max; i++) {
      const token = document.querySelector(`[data-token-id="${i}"]`);
      if (token) {
        token.classList.remove("highlighted");
      }
    }
  }

  function tokenHighlightHandler(event) {
    const target = event.target;
    const tokenId = target.dataset.tokenId;
    if (tokenId !== undefined) {
      // Highlight the current element
      target.classList.add("highlighted");
      // Find and highlight corresponding tokens in all areas
      const allTokens = document.querySelectorAll(
        `[data-token-id="${tokenId}"]`,
      );
      allTokens.forEach((token) => {
        if (token !== target) {
          token.classList.add("highlighted");
        }
      });
      return;
    }

    const startToken = target.dataset.startToken;
    const endToken = target.dataset.endToken;
    if (startToken !== undefined && endToken !== undefined) {
      const startId = parseInt(startToken, 10);
      const endId = parseInt(endToken, 10);
      if (startId > endId) {
        console.warn("Invalid token range:", startId, endId);
        return;
      }
      highlightTokens(startId, endId);
      return;
    }
  }

  function tokenUnhighlightHandler(event) {
    const target = event.target;
    const tokenId = target.dataset.tokenId;

    if (tokenId !== undefined) {
      // Remove highlights from all tokens with this ID
      const allTokens = document.querySelectorAll(
        `[data-token-id="${tokenId}"]`,
      );
      allTokens.forEach((token) => {
        token.classList.remove("highlighted");
      });
    }

    const startToken = target.dataset.startToken;
    const endToken = target.dataset.endToken;
    if (startToken !== undefined && endToken !== undefined) {
      const startId = parseInt(startToken, 10);
      const endId = parseInt(endToken, 10);
      if (startId > endId) {
        console.warn("Invalid token range:", startId, endId);
        return;
      }
      // Unhighlight all tokens in the range
      unhighlightTokens(startId, endId);
      return;
    }
  }

  function tokenClickHandler(event) {
    const target = event.target;
    const tokenId = target.dataset.tokenId;

    if (!tokenId) return;

    console.log("Clicked token ID:", tokenId, "Element:", target);

    // Find corresponding tokens in other panes
    const leftPaneContent = document.getElementById("left-pane-content");
    const rightPaneContent = document.getElementById("right-pane-content");

    // Determine which pane the clicked token is in
    const isInLeftPane = leftPaneContent.contains(target);
    const isInRightPane = rightPaneContent.contains(target);

    console.log(
      "Is in left pane:",
      isInLeftPane,
      "Is in right pane:",
      isInRightPane,
    );

    if (isInLeftPane) {
      // Clicked in left pane, find and scroll to token in right pane
      const rightToken = rightPaneContent.querySelector(
        `[data-token-id="${tokenId}"]`,
      );
      console.log("Looking for right token:", rightToken);
      if (rightToken) {
        scrollToAndFlash(rightToken, rightPaneContent);
      }
    } else if (isInRightPane) {
      // Clicked in right pane, find and scroll to token in left pane
      // Look specifically for source tokens (spans with syntax highlighting classes)
      const leftToken = leftPaneContent.querySelector(
        `span[data-token-id="${tokenId}"]`,
      );
      console.log("Looking for left token:", leftToken);
      if (leftToken) {
        scrollToAndFlash(leftToken, leftPaneContent);
      }
    }
  }

  function scrollToAndFlash(element, container) {
    // Scroll the container to bring the element into view
    const containerRect = container.getBoundingClientRect();
    const elementRect = element.getBoundingClientRect();

    // Calculate if element is outside the visible area
    const isAbove = elementRect.top < containerRect.top;
    const isBelow = elementRect.bottom > containerRect.bottom;

    if (isAbove || isBelow) {
      // Calculate the scroll position to center the element
      const containerScrollTop = container.scrollTop;
      const elementOffsetTop = element.offsetTop;
      const containerHeight = container.clientHeight;
      const elementHeight = element.offsetHeight;

      const scrollTo =
        elementOffsetTop - containerHeight / 2 + elementHeight / 2;

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
    }, 300);
  }

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

  function switchToPane(pane, sectionName) {
    const paneContent = document.getElementById(`${pane}-pane-content`);

    const paneSelector = document.getElementById(`${pane}-selector`);
    if (paneSelector.value !== sectionName) {
      paneSelector.value = sectionName;
    }

    // Find the section in the data sections
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
  setupTokenHighlighting();
});
