app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

main! = |_| Stdout.line!("Hello, world!")
