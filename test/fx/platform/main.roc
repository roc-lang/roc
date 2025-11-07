platform ""
    requires {} { main! : () => {} }
    exposes [Stdout, Stderr, Host]
    packages {}
    provides { main_for_host! }

import Stdout
import Stderr
import Host

main_for_host! : () => {}
main_for_host! = main!
