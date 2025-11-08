platform ""
    requires {} { main! : () => {} }
    exposes [Stdout, Stderr, Stdin, Host]
    packages {}
    provides { main_for_host! }

import Stdout
import Stderr
import Stdin
import Host

main_for_host! : () => {}
main_for_host! = main!
