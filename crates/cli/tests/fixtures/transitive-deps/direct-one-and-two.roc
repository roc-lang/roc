app [main] { 
  pf: platform "../packages/platform/main.roc", 
  one: "one/main.roc",
  two: "two/main.roc",
}

import one.One
import two.Two

main = "$(One.example) | $(Two.example)"
