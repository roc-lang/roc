app [main] {
    pf: platform "../../test-platform-simple-zig/main.roc",
    zero: "zero/main.roc",
}

import zero.Zero

main = Zero.example
