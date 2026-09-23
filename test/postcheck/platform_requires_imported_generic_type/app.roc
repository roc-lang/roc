app [main] { pf: platform "platform/main.roc" }

import pf.Program

State : U64

main = Program.make(1)
