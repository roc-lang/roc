app [main!] { pf: platform "../platform/main.roc" }

import Src/Widget as Widget

main! = || Widget.say!(Widget.default |> Widget.message)
