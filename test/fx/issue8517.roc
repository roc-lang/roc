app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Stdin

main! = || {
  expect 1 == 2
}
