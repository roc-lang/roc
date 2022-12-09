const tutorialTocToggle = document.querySelector("#tutorial-toc-toggle");

document.querySelectorAll("#tutorial-toc li a").forEach((elem) => {
    // Clicking any of the ToC links closes the ToC
    elem.addEventListener("click", (event) => {
        tutorialTocToggle.checked = false;
    })
});

document.addEventListener("keydown", (event) => {
    // Escape closes the ToC
    if (event.key == "Escape") {
        tutorialTocToggle.checked = false;
    }
});