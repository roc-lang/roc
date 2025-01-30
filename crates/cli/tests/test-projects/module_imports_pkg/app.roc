app [main] {
    pkg: "./pkg/main.roc",
}

import Module

main =
    Module.value_from_pkg
