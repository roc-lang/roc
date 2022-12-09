document.querySelectorAll("#tutorial-toc li a").forEach((elem) => {
    elem.addEventListener("click", (event) => {
        document.querySelector("#tutorial-toc-toggle").checked = false;
    })
});