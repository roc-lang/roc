platform ""
    requires {} { main! : () => {} }
    exposes [Stdout, Stderr, Stdin]
    packages {}
    provides { main_for_host!: "main" }
    targets: {
        exe: {
            x64mac: [app],
            arm64mac: [app],
            x64linux: [app],
            arm64linux: [app],
        }
    }

import Stdout
import Stderr
import Stdin

main_for_host! : () => {}
main_for_host! = main!
