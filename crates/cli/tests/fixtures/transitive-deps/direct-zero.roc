app [main] { 
  pf: platform "../packages/platform/main.roc", 
  zero: "zero/main.roc",
}

import zero.Zero

main = Zero.example
