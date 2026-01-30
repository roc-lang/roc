app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.SimpleElement

main! = || {
    Stdout.line!("Test: SimpleElement with Leaf in list")
    leaf_elem = SimpleElement.leaf("test")
    container_elem = SimpleElement.container([leaf_elem])
    SimpleElement.process!(container_elem)
    Stdout.line!("Done!")
}
