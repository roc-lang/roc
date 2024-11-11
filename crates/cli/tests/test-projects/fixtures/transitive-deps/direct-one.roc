app [main] {
    pf: platform "../../test-platform-simple-zig/main.roc",
    one: "one/main.roc",
}

import one.One

main = One.example
