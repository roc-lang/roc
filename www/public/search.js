(function() {
  let sidebar = document.getElementById("sidebar-nav");
  let searchBox = document.getElementById("module-search");

  function search() {
    let text = searchBox.value.toLowerCase(); // Search is case-insensitive.

    if (text === "") {
      // Un-hide everything
      sidebar.querySelectorAll(".sidebar-entry a").forEach((entry) => entry.classList.remove("hidden"));

      // Re-hide all the sub-entries except for those of the first module
      sidebar.querySelectorAll(".sidebar-entry:not(:first-of-type) .sidebar-sub-entries a").forEach((entry) => entry.classList.add("hidden"));
    } else {
      // First, show/hide all the sub-entries within each module (top-level functions etc.)
      sidebar.querySelectorAll(".sidebar-sub-entries a").forEach((entry) => {
        if (entry.textContent.toLowerCase().includes(text)) {
          entry.classList.remove("hidden");
        } else {
          entry.classList.add("hidden");
        }
      });

      // Then, show/hide modules based on whether they match, or any of their sub-entries matched
      sidebar.querySelectorAll(".sidebar-module-link").forEach((entry) => {
        if (entry.textContent.toLowerCase().includes(text) || entry.parentNode.querySelectorAll(".sidebar-sub-entries a:not(.hidden)").length > 0) {
          entry.classList.remove("hidden");
        } else {
          entry.classList.add("hidden");
        }
      });
    }
  }

  searchBox.addEventListener("input", search);

  search();
})();
