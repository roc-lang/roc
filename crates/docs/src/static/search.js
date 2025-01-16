const toggleSidebarEntryActive = (moduleName) => {
  let sidebar = document.getElementById("sidebar-nav");

  if (sidebar != null) {
    // Un-hide everything
    sidebar.querySelectorAll(".sidebar-entry").forEach((entry) => {
      let entryName = entry.querySelector(".sidebar-module-link").dataset
        .moduleName;
      if (moduleName === entryName) {
        entry.firstChild.classList.toggle("active");
      }
    });
  }
};

const setupSidebarNav = () => {
  // Re-hide all the sub-entries except for those of the current module
  let currentModuleName = document.querySelector(".module-name").textContent;
  toggleSidebarEntryActive(currentModuleName);

  document.querySelectorAll(".entry-toggle").forEach((el) => {
    el.addEventListener("click", (e) => {
      e.preventDefault();
      e.stopImmediatePropagation();
      const moduleName = e.target.parentElement.dataset.moduleName;
      toggleSidebarEntryActive(moduleName);
    });
  });
};

const setupSearch = () => {
  let searchTypeAhead = document.getElementById("search-type-ahead");
  let searchBox = document.getElementById("module-search");
  let searchForm = document.getElementById("module-search-form");
  let topSearchResultListItem = undefined;

  // Hide the results whenever anyone clicks outside the search results,
  // or on a specific search result.
  window.addEventListener("click", function (event) {
    if (!searchForm?.contains(event.target) || event.target.closest("#search-type-ahead a")) {
      searchTypeAhead.classList.add("hidden");
    }
  });

  if (searchBox != null) {
    function searchKeyDown(event) {
      switch (event.key) {
        case "ArrowDown": {
          event.preventDefault();

          const focused = document.querySelector(
            "#search-type-ahead > li:not([class*='hidden']) > a:focus",
          );

          // Find the next element to focus.
          let nextToFocus = focused?.parentElement?.nextElementSibling;

          while (
            nextToFocus != null &&
            nextToFocus.classList.contains("hidden")
          ) {
            nextToFocus = nextToFocus.nextElementSibling;
          }

          if (nextToFocus == null) {
            // If none of the links were focused, focus the first one.
            // Also if we've reached the last one in the list, wrap around to the first.
            document
              .querySelector(
                "#search-type-ahead > li:not([class*='hidden']) > a",
              )
              ?.focus();
          } else {
            nextToFocus.querySelector("a").focus();
          }

          break;
        }
        case "ArrowUp": {
          event.preventDefault();

          const focused = document.querySelector(
            "#search-type-ahead > li:not([class*='hidden']) > a:focus",
          );

          // Find the next element to focus.
          let nextToFocus = focused?.parentElement?.previousElementSibling;
          while (
            nextToFocus != null &&
            nextToFocus.classList.contains("hidden")
          ) {
            nextToFocus = nextToFocus.previousElementSibling;
          }

          if (nextToFocus == null) {
            // If none of the links were focused, or we're at the first one, focus the search box again.
            searchBox?.focus();
          } else {
            // If one of the links was focused, focus the previous one
            nextToFocus.querySelector("a").focus();
          }

          break;
        }
        case "Enter": {
          // In case this is just an anchor link (which will move the scroll bar but not
          // reload the page), hide the search bar.
          searchTypeAhead.classList.add("hidden");
          break;
        }
      }
    }

    searchForm.addEventListener("keydown", searchKeyDown);

    function search() {
      topSearchResultListItem = undefined;
      let text = searchBox.value.toLowerCase(); // Search is case-insensitive.

      if (text === "") {
        searchTypeAhead.classList.add("hidden");
      } else {
        let totalResults = 0;
        // Firsttype-ahead-signature", show/hide all the sub-entries within each module (top-level functions etc.)
        searchTypeAhead.querySelectorAll("li").forEach((entry) => {
          const entryModule = entry
            .querySelector(".type-ahead-module-name")
            .textContent.toLowerCase();
          const entryName = entry
            .querySelector(".type-ahead-def-name")
            .textContent.toLowerCase();
          const entrySignature = entry
            .querySelector(".type-ahead-signature")
            ?.textContent?.toLowerCase()
            ?.replace(/\s+/g, "");

          const qualifiedEntryName = `${entryModule}.${entryName}`;

          if (
            qualifiedEntryName.includes(text) ||
            entrySignature?.includes(text.replace(/\s+/g, ""))
          ) {
            totalResults++;
            entry.classList.remove("hidden");
            if (topSearchResultListItem === undefined) {
              topSearchResultListItem = entry;
            }
          } else {
            entry.classList.add("hidden");
          }
        });
        if (totalResults < 1) {
          searchTypeAhead.classList.add("hidden");
        } else {
          searchTypeAhead.classList.remove("hidden");
        }
      }
    }

    searchBox.addEventListener("input", search);

    search();

    function searchSubmit(e) {
      // pick the top result if the user submits search form
      e.preventDefault();
      if (topSearchResultListItem !== undefined) {
        let topSearchResultListItemAnchor =
          topSearchResultListItem.querySelector("a");
        if (topSearchResultListItemAnchor !== null) {
          topSearchResultListItemAnchor.click();
        }
      }
    }
    searchForm.addEventListener("submit", searchSubmit);

    // Capture '/' keypress for quick search
    window.addEventListener("keyup", (e) => {
      if (e.key === "s" && document.activeElement !== searchBox) {
        e.preventDefault();
        searchBox.focus();
        searchBox.value = "";
      }

      if (e.key === "Escape") {
        if (document.activeElement === searchBox) {
          // De-focus and clear input box
          searchBox.value = "";
          searchBox.blur();
        } else {
          // Hide the search results
          searchTypeAhead.classList.add("hidden");

          if (searchTypeAhead.contains(document.activeElement)) {
            searchBox.focus();
          }
        }
      }
    });
  }
};

const isTouchSupported = () => {
  try {
    document.createEvent("TouchEvent");
    return true;
  } catch (e) {
    return false;
  }
};

const setupCodeBlocks = () => {
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
};

const setupSidebarToggle = () => {
  let body = document.body;
  const sidebarOpen = "sidebar-open";
  const removeOpenClass = () => {
    body.classList.remove(sidebarOpen);
    document.body
      .querySelector("main")
      .removeEventListener("click", removeOpenClass);
  };
  Array.from(document.body.querySelectorAll(".menu-toggle")).forEach(
    (menuToggle) => {
      menuToggle.addEventListener("click", (e) => {
        body.classList.toggle(sidebarOpen);
        e.stopPropagation();
        if (body.classList.contains(sidebarOpen)) {
          document.body.addEventListener("click", removeOpenClass);
        }
      });
    },
  );
};

setupSidebarNav();
setupSearch();
setupCodeBlocks();
setupSidebarToggle();
