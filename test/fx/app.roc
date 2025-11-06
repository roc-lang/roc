app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = |_| Stdout.line!("Hello from stdout!")
