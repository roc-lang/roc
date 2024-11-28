(() => {
    let sidebar = document.getElementById("sidebar-nav");

    if (sidebar != null) {
      // Un-hide everything
      sidebar
        .querySelectorAll(".sidebar-entry a")
        .forEach((entry) => entry.classList.remove("hidden"));

      // Re-hide all the sub-entries except for those of the current module
      let currentModuleName = document.querySelector(".module-name").textContent;

      sidebar.querySelectorAll(".sidebar-entry").forEach((entry) => {
        let entryName = entry.querySelector(".sidebar-module-link").textContent;
        if (currentModuleName === entryName) {
          entry.firstChild.classList.add("active");
          return;
        }
        entry
          .querySelectorAll(".sidebar-sub-entries a")
          .forEach((subEntry) => subEntry.classList.add("hidden"));
      });
    }

    let searchTypeAhead = document.getElementById("search-type-ahead");
    let searchBox = document.getElementById("module-search");
    let searchForm = document.getElementById("module-search-form");
    let topSearchResultListItem = undefined;

    if (searchBox != null) {
        function search() {
            topSearchResultListItem = undefined;
            let text = searchBox.value.toLowerCase(); // Search is case-insensitive.

                if (text === "") {
                    searchTypeAhead.classList.add("hidden");
                } else {
                    let totalResults = 0;
                    // Firsttype-ahead-signature", show/hide all the sub-entries within each module (top-level functions etc.)
                    searchTypeAhead.querySelectorAll("li").forEach((entry) => {
                        const entryName = entry
                            .querySelector(".type-ahead-def-name")
                            .textContent.toLowerCase();
                        const entrySignature = entry
                            .querySelector(".type-ahead-signature")
                            .textContent.toLowerCase()
                            .replace(/\s+/g, "");

                        if (
                            totalResults < 5 &&
                            (entryName.includes(text) ||
                                entrySignature.includes(text.replace(/\s+/g, "")))
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
                e.preventDefault;
                searchBox.focus();
                searchBox.value = "";
            }

            if (e.key === "Escape" && document.activeElement === searchBox) {
                e.preventDefault;

                // De-focus input box
                searchBox.blur();
            } else if (
                e.key === "Escape" &&
                searchTypeAhead.contains(document.activeElement)
            ) {
                e.preventDefault;

                // De-focus type ahead
                searchBox.focus();
                searchBox.blur();
            }
        });
    }


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
})();
