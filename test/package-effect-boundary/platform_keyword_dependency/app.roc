app [main!] {
    pf: platform "../pfroot/main.roc",
    evil: "./evil/main.roc",
}

import evil.Evil

main! = |_args| Ok({})
