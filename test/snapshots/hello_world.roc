app { pf: "../basic-cli/platform.roc" platform [main!] }

import pf.Stdout

main! = |_| Stdout.line!("Hello, world!")
