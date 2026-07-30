platform ""
    requires {
        [State : state, Model : model] for program : {
            init! : {
                config : {},
                run! : Host => Try(State(model), [Exit(I64)]),
            },
            render! : State(model), Host => Try(State(model), [Exit(I64), ..]),
        }
    }
    exposes [App, Host]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {}
    targets: {
        inputs_dir: "targets/",
        x64mac: { inputs: [app], output: Archive },
        arm64mac: { inputs: [app], output: Archive },
        x64musl: { inputs: [app], output: Archive },
        arm64musl: { inputs: [app], output: Archive },
        x64glibc: { inputs: [app], output: Archive },
        arm64glibc: { inputs: [app], output: Archive },
        x64win: { inputs: [app], output: Archive },
        arm64win: { inputs: [app], output: Archive },
    }

import App
import Host exposing [Host]

main_for_host! : {} => {}
main_for_host! = |_| {}
