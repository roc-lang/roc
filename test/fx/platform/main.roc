platform ""
    requires {} { main! : () => {} }
    exposes [Stdout, Stderr, Stdin]
    packages {}
    provides { main_for_host!: "main" }

import Stdout
import Stderr
import Stdin

main_for_host! : () => {}
main_for_host! = main!
