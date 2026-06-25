platform ""
    requires {} { main! : U64 => Str }
    exposes [Runtime, HostWrap]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_host_wrap_token": HostWrap.wrap!,
        "roc_runtime_seed": Runtime.seed!,
    }
    targets: {
        inputs_dir: "../platform/targets/",
        wasm32: { inputs: ["host.wasm", app], output: Shared },
    }

import Runtime
import HostWrap

main_for_host! : () => Str
main_for_host! = || main!(Runtime.seed!())
