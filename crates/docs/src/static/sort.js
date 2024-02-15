(() => {
    let sidebar = document.getElementById("sidebar-nav");
    let sortButton = document.getElementById("sort-button");

    if (sortButton != null) {
        function sort() {
            [...sidebar.querySelectorAll(".sidebar-sub-entries")]
                .forEach(entry =>
                    [...entry.querySelectorAll("a")]
                        .sort((a, b) => a.innerText > b.innerText)
                        .forEach(n => entry.appendChild(n)));

        }
        sortButton.onclick = sort;
        }
})();
