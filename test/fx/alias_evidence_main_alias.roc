app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# The platform's root is an alias of its requirement, which the app binds to
# aliases in turn.
run! = || Stdout.line!("ran")

start! = run!

main! = start!
