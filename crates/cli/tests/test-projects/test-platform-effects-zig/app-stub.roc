app [main!] { pf: platform "main.roc" }

import pf.Effect

# just a stubbed app for building the test platform
main! = \{} ->

    Effect.put_line!("")

    {}
