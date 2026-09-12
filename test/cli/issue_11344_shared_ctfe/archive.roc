app [main] { pf: platform "./platform/main.roc" }

import Helper

main = |input| { known: Helper.render("constant"), dynamic: Helper.render(input) }
