app [main] {
    pkg: "./pkg/main.roc",
}

import Module

main =
    Module.valueFromPkg
