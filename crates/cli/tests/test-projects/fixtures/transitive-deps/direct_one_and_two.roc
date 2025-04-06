app [main] {
    pf: platform "../../test-platform-simple-zig/main.roc",
    one: "one/main.roc",
    two: "two/main.roc",
}

import one.One
import two.Two

main = "${One.example} | ${Two.example}"
