app [main] { 
  pf: platform "../packages/platform/main.roc", 
  one: "one/main.roc",
}

import one.One

main = One.example
