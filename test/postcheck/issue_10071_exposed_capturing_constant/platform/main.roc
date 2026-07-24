platform ""
    requires {
        main : {} -> Elem
    }
    exposes [Elem, Browser, Http]
    packages {}
    provides { "roc_ui_init": ui_init }
    hosted {
        "roc_get": HostValue.get_with_capability!,
        "roc_store": HostValue.store_with_capability!,
        "roc_take": HostValue.take_with_capability!,
    }
    targets: {
        inputs_dir: "targets/",
        x64mac: { inputs: [app], output: Archive },
        arm64mac: { inputs: [app], output: Archive },
        x64musl: { inputs: [app], output: Archive },
        arm64musl: { inputs: [app], output: Archive },
        wasm32: { inputs: [app], output: Archive },
    }

import Elem exposing [Elem]
import Browser
import HostValue
import Http

ui_init : {} -> Box(Elem)
ui_init = |_| Box.box(main({}))
