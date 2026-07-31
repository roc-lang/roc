app [main!] { pf: platform "../platform/main.roc" }

import pf.Stdout
import Src.Widget as Widget

main! = || Stdout.line!(Widget.message({}))
