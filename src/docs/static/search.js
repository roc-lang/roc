const setupSearch = () => {
  let searchTypeAhead = document.getElementById("search-type-ahead");
  let searchBox = document.getElementById("module-search");
  let searchForm = document.getElementById("module-search-form");
  let topSearchResultListItem = undefined;
  const mobileSearch = window.matchMedia("(max-width: 768px)");
  const searchDialog = document.createElement("dialog");
  searchDialog.className = "mobile-search-dialog";
  searchDialog.setAttribute("aria-label", "Search documentation");
  document.body.append(searchDialog);
  const backButton = document.createElement("button");
  backButton.type = "button";
  backButton.className = "search-back";
  backButton.textContent = "←";
  backButton.setAttribute("aria-label", "Back to documentation");
  const placeholder = document.createComment("Search form position");
  let closingSearch = false;
  let pageScrollY = 0;
  function closeMobileSearch() {
    if (!searchDialog.open) return;
    closingSearch = true;
    searchTypeAhead.classList.add("hidden");
    searchDialog.close();
    placeholder.replaceWith(searchForm);
    backButton.remove();
    searchBox.blur();
    window.scrollTo(0, pageScrollY);
    closingSearch = false;
  }
  function openMobileSearch() {
    if (!mobileSearch.matches || searchDialog.open || closingSearch) return;
    pageScrollY = window.scrollY;
    searchForm.before(placeholder);
    searchDialog.append(searchForm);
    searchForm.prepend(backButton);
    searchDialog.showModal();
    searchBox.focus({ preventScroll: true });
  }
  backButton.addEventListener("click", closeMobileSearch);
  searchDialog.addEventListener("cancel", event => {
    event.preventDefault();
    closeMobileSearch();
  });
  mobileSearch.addEventListener("change", () => {
    if (!mobileSearch.matches) closeMobileSearch();
  });

  // Keep results and their scroll position when opening a link in another tab.
  // Ordinary result clicks and clicks outside the search still dismiss them.
  window.addEventListener("click", function (event) {
    const result = event.target.closest("#search-type-ahead a");
    if (result && (event.metaKey || event.ctrlKey || event.shiftKey || event.altKey)) return;
    if (!searchForm?.contains(event.target) || result) {
      searchTypeAhead.classList.add("hidden");
      closeMobileSearch();
    }
  }, { capture: true });

  if (searchBox != null) {
    function searchKeyDown(event) {
      if (event.key === "Escape" && searchDialog.open) { event.preventDefault(); closeMobileSearch(); return; }
      if (event.key === "Escape") { searchBox.focus(); searchTypeAhead.classList.add("hidden"); return; }
      if (event.key !== "ArrowDown" && event.key !== "ArrowUp") return;
      event.preventDefault();
      const links = Array.from(searchTypeAhead.querySelectorAll("li:not(.hidden) > a.type-ahead-link, .search-suggestion > button"));
      const current = links.indexOf(document.activeElement);
      const next = event.key === "ArrowDown" ? (current + 1) % links.length : current - 1;
      if (next < 0 || !links.length) searchBox.focus();
      else links[next]?.focus();
    }

    searchForm.addEventListener("keydown", searchKeyDown);

    const engine = window.rocDocsSearch;
    const indexUrl = new URL(searchForm.dataset.searchIndex, location.href);
    const docsBase = new URL("./", indexUrl);
    const categories = new Map([["api", "API"], ["guides", "Guides & examples"]]);
    let allLabel = "All";
    let selectedCategory = "";
    const expandedCategories = new Set();
    let showSnippets = false;
    let prose = [];
    let suggestions = [];
    const apiDocumentation = new Map();
    let loaded = false;
    let loading;
    let failed = false;
    let dynamicItems = [];
    const apiEntries = Array.from(searchTypeAhead.querySelectorAll("li")).map(li => ({
      li,
      href: new URL(li.querySelector("a").getAttribute("href"), location.href).href,
      name: [li.querySelector(".type-ahead-module-name")?.textContent,
        li.querySelector(".type-ahead-def-name")?.textContent].filter(Boolean).join(".").toLowerCase(),
      signature: (li.querySelector(".type-ahead-signature")?.textContent || "").toLowerCase().replace(/\s+/g, ""),
      category: "api",
      kind: li.dataset.searchKind,
    }));
    let shownItems = [];
    const MAX_RESULTS = 50;

    function addDocument(article) {
      const category = article.dataset.category;
      const href = new URL(article.dataset.href, docsBase).href;
      const title = article.dataset.title;
      const fullNavigation = article.hasAttribute("data-full-navigation");
      if (!article.hasAttribute("data-sections")) {
        prose.push({ category, href, title, fullNavigation, text: article.textContent, terms: [] });
        return;
      }
      let section = { category, href, title, fullNavigation, text: "", terms: [] };
      for (const node of article.querySelectorAll("h1,h2,h3,h4,h5,h6,p,pre,li,table")) {
        // Don't count nested list items and paragraphs twice.
        if (node.parentElement.closest("li,pre,table")) continue;
        if (/^H[1-6]$/.test(node.tagName) && node.id) {
          prose.push(section);
          section = { category, fullNavigation, href: `${href.split("#")[0]}#${encodeURIComponent(node.id)}`,
            title: `${title} › ${node.textContent}`, text: "", terms: [] };
        } else section.text += ` ${node.textContent}`;
      }
      prose.push(section);
    }

    function loadIndex() {
      if (loading) return loading;
      loading = fetch(indexUrl, { credentials: "same-origin" }).then(response => {
        if (!response.ok) throw new Error(`Search index: ${response.status}`);
        return response.text();
      }).then(text => {
        const doc = new DOMParser().parseFromString(text, "text/html");
        doc.querySelectorAll("[data-search-categories] [data-category]").forEach(node => {
          categories.delete(node.dataset.category);
          categories.set(node.dataset.category, node.textContent);
        });
        suggestions = Array.from(doc.querySelectorAll("[data-search-suggestion]"), node => ({
          query: node.dataset.searchSuggestion, description: node.textContent,
        }));
        const label = doc.querySelector("[data-search-all-label]");
        if (label) allLabel = label.textContent;
        doc.querySelectorAll("article[data-href]").forEach(addDocument);
        doc.querySelectorAll("[data-search-term]").forEach(node => {
          const href = new URL(node.dataset.href, docsBase).href;
          let entry = prose.find(item => item.href === href && item.category === node.dataset.category);
          if (!entry) {
            entry = { category: node.dataset.category, href, title: node.textContent, text: "", terms: [] };
            prose.push(entry);
          }
          entry.terms.push(node.dataset.searchTerm);
        });
        for (const entry of prose) {
          if (entry.category === "api" && entry.text.trim() && !apiDocumentation.has(entry.href)) {
            apiDocumentation.set(entry.href, entry.text);
          }
        }
        loaded = true;
      }).catch(() => { failed = true; }).then(() => {
        // Never replace a result while someone is navigating it with the keyboard.
        if (!searchTypeAhead.contains(document.activeElement) && document.activeElement === searchBox) search();
      });
      return loading;
    }
    function reopenSearch() {
      if (closingSearch) return;
      openMobileSearch();
      loadIndex();
      if (searchTypeAhead.classList.contains("hidden")) search();
    }
    searchBox.addEventListener("focus", reopenSearch);
    searchBox.addEventListener("click", reopenSearch);

    function dynamic(tag, className, text) {
      const node = document.createElement(tag);
      node.className = className;
      if (text) node.textContent = text;
      return node;
    }
    function appendItem(node) {
      searchTypeAhead.appendChild(node);
      dynamicItems.push(node);
    }
    function addHeading(label) {
      appendItem(dynamic("li", "search-group-label", label));
    }
    function addSnippet(link, text, query, fromStart = false) {
      if (!showSnippets || !text?.trim()) return;
      const snippet = dynamic("small", "search-snippet");
      for (const part of engine.highlightParts(engine.snippet(text, fromStart ? "" : query), query)) {
        snippet.append(part.match ? dynamic("mark", "", part.text) : document.createTextNode(part.text));
      }
      link.append(snippet);
      dynamicItems.push(snippet);
    }
    function addApi(entry, query, fromStart = true) {
      entry.li.classList.remove("hidden");
      searchTypeAhead.appendChild(entry.li);
      shownItems.push(entry.li);
      addSnippet(entry.li.querySelector("a"), apiDocumentation.get(entry.href), query, fromStart);
    }
    function addProse(entry, query) {
      const item = dynamic("li", "search-prose-result");
      const link = dynamic("a", "type-ahead-link type-ahead-langref");
      link.href = entry.href;
      if (entry.fullNavigation) link.setAttribute("data-full-navigation", "");
      link.append(dynamic("span", "search-result-title", entry.title));
      if (entry.matchedTerm && engine.normalize(entry.title) !== engine.normalize(entry.matchedTerm)) {
        link.append(dynamic("small", "search-match-note", ` (search term: ${entry.matchedTerm})`));
      }
      addSnippet(link, entry.text, query);
      item.append(link);
      appendItem(item);
      shownItems.push(item);
    }
    function capturePosition() {
      const top = searchTypeAhead.getBoundingClientRect().top +
        (searchTypeAhead.querySelector(".search-filters")?.offsetHeight || 0);
      const link = Array.from(searchTypeAhead.querySelectorAll("li:not(.hidden) > a.type-ahead-link"))
        .find(node => node.getBoundingClientRect().bottom > top);
      return { scrollTop: searchTypeAhead.scrollTop, href: link?.href,
        offset: link ? link.getBoundingClientRect().top - top : 0 };
    }
    function restorePosition(position) {
      if (!position) { searchTypeAhead.scrollTop = 0; return; }
      searchTypeAhead.scrollTop = position.scrollTop;
      if (!position.href) return;
      const link = Array.from(searchTypeAhead.querySelectorAll("li:not(.hidden) > a.type-ahead-link"))
        .find(node => node.href === position.href);
      if (!link) return;
      const top = searchTypeAhead.getBoundingClientRect().top +
        (searchTypeAhead.querySelector(".search-filters")?.offsetHeight || 0);
      const rect = link.getBoundingClientRect();
      // If a visible snippet disappears, keep its result from scrolling entirely out of view.
      const offset = Math.max(position.offset, -rect.height + 1);
      searchTypeAhead.scrollTop += rect.top - top - offset;
    }
    function addExpansion(group, groupLabel, count, expanded) {
      const row = dynamic("li", "search-more");
      const button = dynamic("button", "", expanded ? `Less ${groupLabel} matches` : `More ${groupLabel} matches (${count})`);
      button.type = "button";
      row.dataset.group = group;
      button.setAttribute("aria-expanded", String(expanded));
      button.addEventListener("click", event => {
        event.stopPropagation();
        const scrollTop = searchTypeAhead.scrollTop;
        const offset = row.getBoundingClientRect().top - searchTypeAhead.getBoundingClientRect().top;
        if (expanded) expandedCategories.delete(group);
        else expandedCategories.add(group);
        search({ scrollTop });
        if (expanded) {
          const replacement = Array.from(searchTypeAhead.querySelectorAll(".search-more"))
            .find(node => node.dataset.group === group);
          searchTypeAhead.scrollTop += replacement.getBoundingClientRect().top -
            searchTypeAhead.getBoundingClientRect().top - offset;
          replacement.querySelector("button").focus({ preventScroll: true });
        } else searchBox.focus({ preventScroll: true });
      });
      row.append(button);
      appendItem(row);
    }
    function search(position) {
      topSearchResultListItem = undefined;
      for (const item of shownItems) item.classList.add("hidden");
      for (const item of dynamicItems) item.remove();
      shownItems = [];
      dynamicItems = [];
      const query = searchBox.value.trim();
      if (!query && document.activeElement !== searchBox && !searchDialog.open) {
        searchTypeAhead.classList.add("hidden");
        return;
      }
      loadIndex();
      if (!query) {
        appendItem(dynamic("li", "search-status", suggestions.length ? "Try searching for…" :
          loaded ? "Search types, functions, and documentation." :
          failed ? "Search types and functions." : "Loading search suggestions…"));
        const groups = new Map();
        for (const suggestion of suggestions) {
          let row = groups.get(suggestion.description);
          if (!row) {
            row = dynamic("li", "search-suggestion");
            row.append(dynamic("span", "search-suggestion-label", `${suggestion.description}: `));
            groups.set(suggestion.description, row);
            appendItem(row);
          } else row.append(document.createTextNode(", "));

          const button = dynamic("button", "");
          button.type = "button";
          button.append(dynamic("code", "", suggestion.query));
          button.addEventListener("click", () => {
            searchBox.value = suggestion.query;
            selectedCategory = "";
            expandedCategories.clear();
            search();
            searchBox.focus({ preventScroll: true });
          });
          row.append(button);
        }
        searchTypeAhead.classList.toggle("hidden", document.activeElement !== searchBox && !searchDialog.open);
        restorePosition(position);
        return;
      }
      const apiMatches = engine.rankApi(apiEntries, query);
      const ranked = engine.rank(prose, query, { allowAliasPrefixes: apiMatches.length === 0 });
      const curatedSymbol = !/[\p{L}\p{N}]/u.test(query) && ranked.some(item => item.exactTerm);
      const used = new Set([...(apiEntries.length ? ["api"] : []), ...prose.map(item => item.category)]);
      if (loaded && (used.size > 1 || prose.length)) {
        const controls = dynamic("li", "search-filters");
        controls.setAttribute("aria-label", "Search options");
        for (const [id, label] of (used.size > 1 ? [["", allLabel], ...Array.from(categories).filter(([id]) => used.has(id))] : [])) {
          const button = dynamic("button", "", label);
          button.type = "button";
          button.setAttribute("aria-pressed", String(id === selectedCategory));
          button.addEventListener("click", event => {
            event.stopPropagation();
            if (selectedCategory === id) return;
            const position = capturePosition();
            selectedCategory = id;
            search(position);
            searchBox.focus({ preventScroll: true });
          });
          controls.append(button);
        }
        if (prose.length) {
          const toggle = dynamic("button", "search-snippet-toggle", "");
          toggle.setAttribute("role", "switch");
          toggle.setAttribute("aria-label", "Snippets");
          const track = dynamic("span", "search-snippet-switch");
          track.setAttribute("aria-hidden", "true");
          toggle.append(track, document.createTextNode("Snippets"));
          toggle.type = "button";
          toggle.setAttribute("aria-checked", String(showSnippets));
          toggle.addEventListener("click", event => {
            event.stopPropagation();
            const position = capturePosition();
            showSnippets = !showSnippets;
            search(position);
            searchTypeAhead.querySelector(".search-snippet-toggle").focus({ preventScroll: true });
          });
          controls.append(toggle);
        }
        appendItem(controls);
      }
      // Explicit symbols/synonyms take precedence; names and signatures otherwise remain first.
      const curated = ranked.filter(item => item.matchedTerm && (!selectedCategory || item.category === selectedCategory));
      const seen = new Set();
      for (const item of curated.slice(0, MAX_RESULTS)) {
        const api = apiEntries.find(entry => entry.href === item.href);
        if (api) {
          addApi(api, query);
          const note = dynamic("small", "search-match-note", ` (alias: ${item.matchedTerm})`);
          api.li.querySelector(".type-ahead-def-name").after(note);
          dynamicItems.push(note);
        } else addProse(item, query);
        seen.add(item.href);
      }
      if (!curatedSymbol && (!selectedCategory || selectedCategory === "api")) {
        const nameMatches = apiMatches.filter(item => !seen.has(item.href));
        const apiMatchHrefs = new Set(apiMatches.map(item => item.href));
        const documentationMatches = ranked.filter(item => item.category === "api" &&
          !seen.has(item.href) && !apiMatchHrefs.has(item.href));
        const matches = [
          ...nameMatches.map(entry => ({ entry, fromStart: true })),
          ...documentationMatches.map(documentation => ({
            entry: apiEntries.find(entry => entry.href === documentation.href),
            documentation,
            fromStart: false,
          })),
        ];
        const expanded = expandedCategories.has("api");
        if (matches.length && loaded && used.size > 1) addHeading(categories.get("api"));
        for (const match of matches.slice(0, expanded ? matches.length : MAX_RESULTS)) {
          if (match.entry) addApi(match.entry, query, match.fromStart);
          else addProse(match.documentation, query);
        }
        if (matches.length > MAX_RESULTS) addExpansion("api", "API", matches.length, expanded);
      }
      for (const [category, label] of categories) {
        if (category === "api" || (selectedCategory && category !== selectedCategory)) continue;
        const matches = ranked.filter(item => item.category === category && !seen.has(item.href));
        if (!matches.length) continue;
        addHeading(label);
        const expanded = expandedCategories.has(category);
        const limit = expanded ? matches.length : 5;
        for (const item of matches.slice(0, limit)) { addProse(item, query); seen.add(item.href); }
        if (matches.length > 5) {
          addExpansion(category, label, matches.length, expanded);
        }
      }
      if (!loaded) appendItem(dynamic("li", "search-status", failed ? "Additional documentation search is unavailable. API search still works." : "Searching documentation…"));
      if (!shownItems.length && loaded) appendItem(dynamic("li", "search-status", "No matching documentation."));
      topSearchResultListItem = shownItems[0];
      searchTypeAhead.classList.remove("hidden");
      restorePosition(position);
    }
    searchBox.addEventListener("input", () => { expandedCategories.clear(); search(); });
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

    // Capture 's' keypress for quick search
    window.addEventListener("keyup", (e) => {
      if (e.key === "s" && document.activeElement !== searchBox) {
        e.preventDefault();
        searchBox.value = "";
        searchBox.focus();
        search();
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

const closestElement = (target, selector) =>
  target?.closest?.(selector) ?? target?.parentElement?.closest?.(selector);

const clickedLink = (event) => {
  for (const node of event.composedPath?.() ?? []) {
    if (node?.matches?.("a[href]")) return node;
  }

  return closestElement(event.target, "a[href]");
};

const setupCodeBlocks = (root = document) => {
  // Select all <samp> elements that are children of <pre> elements
  const codeBlocks = root.querySelectorAll("pre > samp");

  // Iterate over each code block
  codeBlocks.forEach((codeBlock) => {
    if (codeBlock.dataset.copyButtonReady === "true") return;
    codeBlock.dataset.copyButtonReady = "true";

    // Create a "Copy" button
    const copyButton = document.createElement("button");
    copyButton.classList.add("copy-button");
    copyButton.textContent = "Copy";

    // Create a container for the copy button and append it to the document
    const buttonContainer = document.createElement("div");
    buttonContainer.classList.add("button-container");
    buttonContainer.appendChild(copyButton);
    codeBlock.parentNode.insertBefore(buttonContainer, codeBlock);

  });
};

const setupCopyButtonActions = () => {
  document.addEventListener("click", (event) => {
    const copyButton = closestElement(event.target, ".copy-button");
    if (!copyButton) return;

    const codeBlock = copyButton
      .closest(".button-container")
      ?.nextElementSibling;
    if (!codeBlock?.matches?.("samp")) return;

    navigator.clipboard.writeText(codeBlock.innerText);
    copyButton.textContent = "Copied!";
    copyButton.classList.add("copy-button-copied");
  });

  document.addEventListener(
    "pointerleave",
    (event) => {
      if (!event.target?.matches?.(".copy-button")) return;

      event.target.textContent = "Copy";
      event.target.classList.remove("copy-button-copied");
    },
    true,
  );
};

const setupSidebarToggle = () => {
  let body = document.body;
  const sidebarOpen = "sidebar-open";
  const removeOpenClass = (event) => {
    // Toggling a module's disclosure triangle only expands/collapses that
    // entry's sub-list; it must not also close the whole mobile sidebar, so
    // ignore clicks that landed on a toggle rather than outside the sidebar.
    if (closestElement(event.target, ".sidebar-module-summary")) return;

    body.classList.remove(sidebarOpen);
    document.body.removeEventListener("click", removeOpenClass);
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

const docsScriptRootPath = (() => {
  const scriptUrl = document.currentScript?.src;
  if (!scriptUrl) return "/";

  const path = new URL(scriptUrl, window.location.href).pathname;
  return path.replace(/search\.js$/, "");
})();

const setupDocsSoftNavigation = () => {
  const docsRootPath = docsScriptRootPath;
  const mainSelector = "main";
  const contentSelector = ".main-content";
  let activeNavController = undefined;
  let navigationId = 0;
  let loaderResetTimer = undefined;
  let loaderShowTimer = undefined;
  let scrollStateTimer = undefined;
  let mainScrollBox = document.querySelector(mainSelector);
  let cachedMainScrollTop = mainScrollBox?.scrollTop ?? 0;
  let cachedSidebarScrollTop = 0;
  const pageCache = new Map();
  const pageCacheOrder = [];
  const maxCachedPages = 32;

  if (!mainScrollBox?.querySelector(".main-content") || !window.DOMParser) return;

  if ("scrollRestoration" in history) {
    history.scrollRestoration = "manual";
  }

  const docsUrl = (href, base = window.location.href) => {
    let url;

    try {
      url = new URL(href, base);
    } catch (_) {
      return undefined;
    }

    if (url.origin !== window.location.origin) return undefined;
    if (!url.pathname.startsWith(docsRootPath)) return undefined;

    return url;
  };

  const fetchKey = (url) => {
    const key = new URL(url.href);
    key.hash = "";
    return key.href;
  };

  const cachedPage = (url) => pageCache.get(fetchKey(url));

  const rememberPage = (url, page) => {
    const key = fetchKey(url);
    if (!pageCache.has(key)) {
      pageCacheOrder.push(key);
    }

    pageCache.set(key, page);

    while (pageCacheOrder.length > maxCachedPages) {
      const oldestKey = pageCacheOrder.shift();
      pageCache.delete(oldestKey);
    }
  };

  const canonicalDocsPath = (pathname) =>
    pathname
      .replace(/\/index\.html$/, "")
      .replace(/\/$/, "");

  // The docs landing page (this site's root, e.g. "/docs/main/" with nothing
  // after it) shows the guide links (Tutorial, FAQ, Language Reference) above
  // the search bar, in addition to the sidebar; every other docs page (a
  // module, or a langref article) hides those guide links. The server already
  // renders the right state into the initial HTML (see the "docs-index" body
  // class in render_html.zig); this mirrors that check so soft navigation
  // keeps it in sync without a full page load.
  const isDocsIndexPath = (pathname) =>
    canonicalDocsPath(pathname) === canonicalDocsPath(docsRootPath);

  const fragmentKey = (url) =>
    `${url.origin}${canonicalDocsPath(url.pathname)}${url.search}`;

  const sameDocumentFragmentLink = (link, url) => {
    const href = link.getAttribute("href")?.trim() ?? "";
    const hasFragmentIntent = url.hash !== "" || href.startsWith("#");
    const current = new URL(window.location.href);

    return (
      hasFragmentIntent &&
      fragmentKey(url) === fragmentKey(current)
    );
  };
  let activeDocumentKey = fragmentKey(new URL(window.location.href));

  const eligibleLinkUrl = (link) => {
    if (!link || link.hasAttribute("download") || link.hasAttribute("data-full-navigation")) return undefined;

    const target = link.getAttribute("target");
    if (target && target !== "_self") return undefined;

    return docsUrl(link.getAttribute("href"));
  };

  const normalizeDocsLinks = (root, base) => {
    root.querySelectorAll("a[href]").forEach((link) => {
      const url = docsUrl(link.getAttribute("href"), base);
      if (!url) return;

      link.setAttribute("href", `${url.pathname}${url.search}${url.hash}`);
    });
  };

  const getSidebarScrollBox = () =>
    document.querySelector("#sidebar-nav .module-links-container");

  const getMainScrollTop = () => cachedMainScrollTop;

  const setMainScrollTop = (scrollTop) => {
    const main = document.querySelector(mainSelector);
    if (main && cachedMainScrollTop !== scrollTop) {
      main.scrollTop = scrollTop;
      cachedMainScrollTop = scrollTop;
    }
  };

  const getSidebarScrollTop = () => cachedSidebarScrollTop;

  const setSidebarScrollTop = (scrollTop) => {
    const scrollBox = getSidebarScrollBox();
    if (scrollBox && cachedSidebarScrollTop !== scrollTop) {
      scrollBox.scrollTop = scrollTop;
      cachedSidebarScrollTop = scrollTop;
    }
  };

  const currentHistoryState = () => ({
    ...(history.state ?? {}),
    scrollY: getMainScrollTop(),
    sidebarScrollTop: getSidebarScrollTop(),
  });

  const saveCurrentHistoryState = () => {
    history.replaceState(currentHistoryState(), "", window.location.href);
  };

  const scheduleScrollStateSave = () => {
    clearTimeout(scrollStateTimer);

    scrollStateTimer = setTimeout(() => {
      saveCurrentHistoryState();
    }, 200);
  };

  const updateMainScrollState = () => {
    cachedMainScrollTop = document.querySelector(mainSelector)?.scrollTop ?? 0;
    scheduleScrollStateSave();
  };

  const updateSidebarScrollState = () => {
    cachedSidebarScrollTop = getSidebarScrollBox()?.scrollTop ?? 0;
    scheduleScrollStateSave();
  };

  const bindMainScrollBox = () => {
    const nextMainScrollBox = document.querySelector(mainSelector);
    if (mainScrollBox === nextMainScrollBox) return;

    mainScrollBox?.removeEventListener("scroll", updateMainScrollState);
    mainScrollBox = nextMainScrollBox;
    mainScrollBox?.addEventListener("scroll", updateMainScrollState, {
      passive: true,
    });
    cachedMainScrollTop = mainScrollBox?.scrollTop ?? 0;
  };

  const ensureLoader = (parent = document.querySelector(mainSelector)) => {
    let loader = document.getElementById("docs-loader");

    if (!loader) {
      loader = document.createElement("div");
      loader.id = "docs-loader";
      loader.setAttribute("aria-hidden", "true");
    }

    if (parent && loader.parentElement !== parent) {
      parent.appendChild(loader);
    }

    return loader;
  };

  const startLoader = () => {
    const loader = ensureLoader();
    clearTimeout(loaderResetTimer);
    clearTimeout(loaderShowTimer);
    loader.classList.remove("is-done");
    loader.classList.remove("is-loading");

    loaderShowTimer = setTimeout(() => {
      loader.classList.add("is-loading");
      loaderShowTimer = undefined;
    }, 1000);
  };

  const finishLoader = () => {
    const loader = ensureLoader();
    clearTimeout(loaderShowTimer);
    loaderShowTimer = undefined;

    const wasVisible = loader.classList.contains("is-loading");
    loader.classList.remove("is-loading");
    if (!wasVisible) {
      loader.classList.remove("is-done");
      return;
    }

    loader.classList.add("is-done");

    clearTimeout(loaderResetTimer);
    loaderResetTimer = setTimeout(() => {
      loader.classList.remove("is-done");
    }, 180);
  };

  const markerBytes = (kind) =>
    new Uint8Array([60, 33, 45, 45, 33, kind, 45, 45, 62]);
  // Generated docs pages use invisible bang-comments as byte-level stream
  // markers: NUL starts .main-content, Unit Separator flushes a complete
  // appendable/highlightable chunk, and Record Separator ends the stream.
  const streamMarkers = [
    { type: "start", bytes: markerBytes(0x00) },
    { type: "chunk", bytes: markerBytes(0x1f) },
    { type: "end", bytes: markerBytes(0x1e) },
  ];
  const markerTailLength =
    Math.max(...streamMarkers.map((marker) => marker.bytes.length)) - 1;

  const concatBytes = (left, right) => {
    if (left.length === 0) return right;
    if (right.length === 0) return left;

    const combined = new Uint8Array(left.length + right.length);
    combined.set(left);
    combined.set(right, left.length);
    return combined;
  };

  const byteSequenceAt = (bytes, sequence, index) => {
    if (index + sequence.length > bytes.length) return false;

    for (let offset = 0; offset < sequence.length; offset += 1) {
      if (bytes[index + offset] !== sequence[offset]) return false;
    }

    return true;
  };

  const findStreamMarker = (bytes) => {
    for (let index = 0; index < bytes.length; index += 1) {
      for (const marker of streamMarkers) {
        if (byteSequenceAt(bytes, marker.bytes, index)) {
          return { ...marker, index };
        }
      }
    }

    return undefined;
  };

  const decodeHtmlEntities = (text) => {
    const element = document.createElement("textarea");
    element.innerHTML = text;
    return element.value;
  };

  const titleFromBytes = (bytes) => {
    const html = new TextDecoder().decode(bytes);
    const match = html.match(/<title[^>]*>([\s\S]*?)<\/title>/i);
    return match ? decodeHtmlEntities(match[1].trim()) : undefined;
  };

  const appendPreviewBytes = (current, bytes, limit = 65536) => {
    if (current.length >= limit || bytes.length === 0) return current;

    const available = Math.min(bytes.length, limit - current.length);
    return concatBytes(current, bytes.subarray(0, available));
  };

  const fetchDocsResponse = (url, signal) => {
    const key = fetchKey(url);
    return fetch(key, { credentials: "same-origin", signal }).then((response) => {
        if (!response.ok) {
          throw new Error(`Could not load ${url.pathname}: ${response.status}`);
        }

        return response;
      });
  };

  const appendMainHtmlChunk = (html, url, content) => {
    const template = document.createElement("template");
    template.innerHTML = html;
    template.content.querySelectorAll("script").forEach((script) => script.remove());
    normalizeDocsLinks(template.content, fetchKey(url));
    setupCodeBlocks(template.content);
    const nodes = Array.from(template.content.childNodes);
    content.appendChild(template.content);
    return nodes;
  };

  const renderMainHtmlChunk = async (html, url, content, signal) => {
    if (html.length === 0) return { appendMs: 0, highlightMs: 0 };

    const appendStart = performance.now();
    const nodes = appendMainHtmlChunk(html, url, content);
    const appendMs = performance.now() - appendStart;

    const highlightStart = performance.now();
    window.rocSyntax?.highlightNodes?.(nodes);
    const highlightMs = performance.now() - highlightStart;

    await new Promise((resolve) => requestAnimationFrame(resolve));

    if (signal.aborted) {
      throw new DOMException("Navigation aborted", "AbortError");
    }

    return { appendMs, highlightMs };
  };

  const renderCachedMainContent = async (page, url, content, signal) => {
    let appendMs = 0;
    let highlightMs = 0;

    for (const html of page.htmlChunks) {
      const stats = await renderMainHtmlChunk(html, url, content, signal);
      appendMs += stats.appendMs;
      highlightMs += stats.highlightMs;
    }

    return {
      title: page.title,
      chunks: page.htmlChunks.length,
      bytesDecoded: page.bytesDecoded,
      appendMs,
      highlightMs,
      fromCache: true,
    };
  };

  const streamMainContent = async (response, url, content, signal) => {
    if (!response.body) {
      throw new Error("Readable response streams are not available");
    }

    const reader = response.body.getReader();
    let pending = new Uint8Array(0);
    let decoder = new TextDecoder();
    let titlePreview = new Uint8Array(0);
    let parts = [];
    let started = false;
    let title = undefined;
    let chunks = 0;
    let bytesDecoded = 0;
    let appendMs = 0;
    let highlightMs = 0;
    const htmlChunks = [];

    const readBodyPart = (bytes) => {
      if (bytes.length === 0) return;

      bytesDecoded += bytes.length;
      const text = decoder.decode(bytes, { stream: true });
      if (text) parts.push(text);
    };

    const flushChunk = async () => {
      const tail = decoder.decode();
      if (tail) parts.push(tail);

      const html = parts.join("");
      parts = [];
      decoder = new TextDecoder();

      if (html.length === 0) return;

      htmlChunks.push(html);
      const stats = await renderMainHtmlChunk(html, url, content, signal);
      appendMs += stats.appendMs;
      highlightMs += stats.highlightMs;
      chunks += 1;
    };

    while (true) {
      const { value, done } = await reader.read();

      if (done) break;

      pending = concatBytes(pending, value);

      while (pending.length > 0) {
        const marker = findStreamMarker(pending);

        if (!marker) {
          const safeLength = pending.length - markerTailLength;
          if (safeLength <= 0) break;

          const safeBytes = pending.subarray(0, safeLength);
          if (started) {
            readBodyPart(safeBytes);
          } else {
            titlePreview = appendPreviewBytes(titlePreview, safeBytes);
          }
          pending = pending.subarray(safeLength);
          break;
        }

        const beforeMarker = pending.subarray(0, marker.index);
        if (started) {
          readBodyPart(beforeMarker);
        } else {
          titlePreview = appendPreviewBytes(titlePreview, beforeMarker);
        }
        pending = pending.subarray(marker.index + marker.bytes.length);

        if (!started) {
          if (marker.type === "start") {
            started = true;
            title = titleFromBytes(titlePreview);
          }
          continue;
        }

        if (marker.type === "chunk") {
          await flushChunk();
        } else if (marker.type === "end") {
          await flushChunk();
          await reader.cancel();
          return { title, chunks, bytesDecoded, appendMs, highlightMs, htmlChunks };
        }
      }
    }

    if (!started) {
      throw new Error("Could not find docs stream start marker");
    }

    readBodyPart(pending);
    await flushChunk();
    return { title, chunks, bytesDecoded, appendMs, highlightMs, htmlChunks };
  };

  const loadMainContent = async (url, content, signal) => {
    const page = cachedPage(url);
    if (page) {
      return renderCachedMainContent(page, url, content, signal);
    }

    const response = await fetchDocsResponse(url, signal);
    const stats = await streamMainContent(response, url, content, signal);
    rememberPage(url, {
      title: stats.title,
      bytesDecoded: stats.bytesDecoded,
      htmlChunks: stats.htmlChunks,
    });

    return stats;
  };

  const syncSidebarForUrl = (targetUrl) => {
    const currentSidebar = document.getElementById("sidebar-nav");
    if (!currentSidebar) return;

    currentSidebar
      .querySelectorAll(".sidebar-module-link[data-module-name]")
      .forEach((link) => {
        const moduleName = link.getAttribute("data-module-name");
        if (moduleName?.startsWith("__")) return;

        const linkUrl = docsUrl(link.getAttribute("href"), window.location.href);
        if (!linkUrl) return;

        const isActive =
          linkUrl.pathname === targetUrl.pathname &&
          linkUrl.search === targetUrl.search;

        link.classList.toggle("active", isActive);

        const details = link.parentElement?.querySelector(":scope > details");
        if (details) details.open = isActive;
      });

    currentSidebar.querySelectorAll(".sidebar-value[href]").forEach((link) => {
      const url = docsUrl(link.getAttribute("href"), window.location.href);
      if (!url) return;

      link.classList.toggle(
        "active",
        url.pathname === targetUrl.pathname &&
          url.search === targetUrl.search &&
          url.hash === targetUrl.hash,
      );
    });
  };

  const hashTarget = (hash) => {
    if (!hash) return undefined;

    let id = hash.slice(1);
    try {
      id = decodeURIComponent(id);
    } catch (_) {
      return undefined;
    }

    return document.getElementById(id);
  };

  const scrollToDestination = (url, restoreScroll) => {
    if (typeof restoreScroll === "number") {
      setMainScrollTop(restoreScroll);
      return;
    }

    const target = hashTarget(url.hash);
    if (target) {
      target.scrollIntoView();
      cachedMainScrollTop = mainScrollBox?.scrollTop ?? 0;
    }
  };

  const restoreSameDocumentLocation = (url, state) => {
    if (typeof state.scrollY === "number") {
      setMainScrollTop(state.scrollY);
    } else {
      scrollToDestination(url);
    }

    if (typeof state.sidebarScrollTop === "number") {
      setSidebarScrollTop(state.sidebarScrollTop);
    }
  };

  const hideSearchResults = () => {
    document.getElementById("search-type-ahead")?.classList.add("hidden");
  };

  const closeSidebar = () => {
    document.body.classList.remove("sidebar-open");
  };

  const logTiming = (label, start) => {
    // console.log(`[docs-nav] ${label} ${(performance.now() - start).toFixed(1)}ms`);
  };

  const createMainShell = (oldMain, oldContent, includePersistentUi) => {
    const nextMain = oldMain.cloneNode(false);
    const nextContent = oldContent.cloneNode(false);
    const currentGuideLinks = document.getElementById("index-guide-links");
    const currentSearch = document.getElementById("module-search-form");

    nextContent.textContent = "";
    nextContent.removeAttribute("data-roc-highlight-id");

    // Carried forward like the search form below: both are positioned via
    // explicit CSS grid-row (not DOM order), so visibility is what determines
    // whether the guide links show, not append order.
    if (includePersistentUi && currentGuideLinks) {
      nextMain.appendChild(currentGuideLinks);
    }

    if (includePersistentUi && currentSearch) {
      nextMain.appendChild(currentSearch);
    }

    nextMain.appendChild(nextContent);

    if (includePersistentUi) {
      ensureLoader(nextMain);
    }

    return { nextMain, nextContent };
  };

  const activateMainShell = (oldMain, oldContent, nextMain, nextContent, url) => {
    window.rocSyntax?.clear(oldContent, false);
    oldMain.replaceWith(nextMain);
    bindMainScrollBox();
    activeDocumentKey = fragmentKey(url);
    document.body.classList.toggle("docs-index", isDocsIndexPath(url.pathname));
    syncSidebarForUrl(url);
    hideSearchResults();
    closeSidebar();
    nextContent.setAttribute("tabindex", "-1");
    nextContent.focus({ preventScroll: true });
  };

  const navigate = async (href, options = {}) => {
    const url = docsUrl(href);
    if (!url) {
      window.location.href = href;
      return;
    }

    navigationId += 1;
    const thisNavigation = navigationId;

    if (activeNavController) {
      activeNavController.abort();
    }

    activeNavController = new AbortController();
    const signal = activeNavController.signal;
    const oldMain = document.querySelector(mainSelector);
    const oldContent = oldMain?.querySelector(contentSelector);
    const sidebarScrollTop =
      options.sidebarScrollTop ?? getSidebarScrollTop();
    const stageUntilReady =
      typeof options.scrollY === "number" && options.history !== "push";
    const totalStart = performance.now();
    // console.log(`[docs-nav] start ${url.pathname}${url.hash}`);

    try {
      if (!oldMain || !oldContent) {
        throw new Error(`Could not find docs main content`);
      }

      startLoader();
      oldMain.setAttribute("aria-busy", "true");

      let phaseStart = performance.now();
      const { nextMain, nextContent } = createMainShell(
        oldMain,
        oldContent,
        !stageUntilReady,
      );
      logTiming("create shell", phaseStart);

      if (!stageUntilReady) {
        phaseStart = performance.now();
        activateMainShell(oldMain, oldContent, nextMain, nextContent, url);
        logTiming("activate shell", phaseStart);
      }

      if (options.history === "push") {
        phaseStart = performance.now();
        const state = { sidebarScrollTop };
        if (!url.hash) state.scrollY = 0;
        history.pushState(state, "", url.href);
        logTiming("push state", phaseStart);
      }

      phaseStart = performance.now();
      const streamStats = await loadMainContent(url, nextContent, signal);
      if (thisNavigation !== navigationId) return;
      if (streamStats.title) document.title = streamStats.title;
      // console.log(
      //   `[docs-nav] stream chunks=${streamStats.chunks} cache=${streamStats.fromCache === true} bytes=${streamStats.bytesDecoded} append=${streamStats.appendMs.toFixed(1)}ms highlight=${streamStats.highlightMs.toFixed(1)}ms`,
      // );
      logTiming("load content", phaseStart);

      if (stageUntilReady) {
        phaseStart = performance.now();
        // Mirrors createMainShell's persistent-UI handling above: staged
        // navigations skip that step (includePersistentUi is false while
        // content streams into a still-hidden shell) and instead move these
        // elements over here, once, right before the shell swap.
        const currentGuideLinksAfterStream =
          document.getElementById("index-guide-links");
        if (currentGuideLinksAfterStream) {
          nextMain.insertBefore(currentGuideLinksAfterStream, nextContent);
        }
        const currentSearchAfterStream =
          document.getElementById("module-search-form");
        if (currentSearchAfterStream) {
          nextMain.insertBefore(currentSearchAfterStream, nextContent);
        }
        ensureLoader(nextMain);
        logTiming("swap restored shell", phaseStart);

        phaseStart = performance.now();
        activateMainShell(oldMain, oldContent, nextMain, nextContent, url);
        logTiming("activate restored shell", phaseStart);
      }

      nextMain.removeAttribute("aria-busy");

      phaseStart = performance.now();
      scrollToDestination(url, options.scrollY);
      logTiming("main scroll", phaseStart);

      phaseStart = performance.now();
      setSidebarScrollTop(sidebarScrollTop);
      logTiming("sidebar scroll", phaseStart);
      logTiming("total", totalStart);
    } catch (error) {
      if (thisNavigation !== navigationId) return;
      if (error.name === "AbortError") return;

      window.location.href = url.href;
    } finally {
      if (thisNavigation === navigationId) {
        activeNavController = undefined;
        finishLoader();
      }
    }
  };

  normalizeDocsLinks(document, window.location.href);
  cachedSidebarScrollTop = getSidebarScrollBox()?.scrollTop ?? 0;
  mainScrollBox?.addEventListener("scroll", updateMainScrollState, {
    passive: true,
  });
  saveCurrentHistoryState();

  getSidebarScrollBox()?.addEventListener("scroll", updateSidebarScrollState, {
    passive: true,
  });

  document.addEventListener("click", (event) => {
    if (
      event.defaultPrevented ||
      event.button !== 0 ||
      event.metaKey ||
      event.ctrlKey ||
      event.shiftKey ||
      event.altKey
    ) {
      return;
    }

    const link = clickedLink(event);
    const url = eligibleLinkUrl(link);
    if (!url) return;
    if (sameDocumentFragmentLink(link, url)) {
      saveCurrentHistoryState();
      return;
    }

    event.preventDefault();
    saveCurrentHistoryState();
    navigate(url.href, { history: "push" });
  });

  window.addEventListener("popstate", (event) => {
    const state = event.state ?? {};
    const url = docsUrl(window.location.href);

    if (url && fragmentKey(url) === activeDocumentKey) {
      restoreSameDocumentLocation(url, state);
      return;
    }

    navigate(window.location.href, {
      history: "replace",
      scrollY: state.scrollY,
      sidebarScrollTop: state.sidebarScrollTop,
    });
  });
};

// Only run setup functions if their required elements are present
if (document.getElementById("module-search")) {
  setupSearch();
}

setupCodeBlocks();
setupCopyButtonActions();
setupDocsSoftNavigation();

if (document.querySelector(".menu-toggle")) {
  setupSidebarToggle();
}
