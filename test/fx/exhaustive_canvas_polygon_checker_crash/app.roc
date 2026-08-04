app [main!] { pf: platform "../platform/main.roc" }

import pf.Stdout
import Render

main! = || {
	Stdout.line!("ok")
}
