app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect
import Menu { echo: \str -> Effect.putLine! str }

main! = \{} ->
    Menu.menu "Agus"
