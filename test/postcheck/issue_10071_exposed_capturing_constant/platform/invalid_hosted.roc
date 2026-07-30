platform ""
    requires {
        main : {} -> Elem
    }
    exposes [Elem, Browser]
    packages {}
    provides { "roc_ui_init": ui_init }
    hosted {}
    targets: {
        inputs_dir: "targets/",
        x64mac: { inputs: [app], output: Archive },
        arm64mac: { inputs: [app], output: Archive },
        x64musl: { inputs: [app], output: Archive },
        arm64musl: { inputs: [app], output: Archive },
    }

import Elem exposing [Elem]
import Browser

ui_init : {} -> Box(Elem)
ui_init = |_| Box.box(main({}))
