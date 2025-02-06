app [main] { pf: platform "c-platform/main.roc" }

# may require:
# ubuntu: sudo apt install build-essential clang
# fedora: sudo dnf install clang

# run with `roc --build-host --suppress-build-host-warning roc_loves_c.roc`

main = "Roc <3 C!\n"
