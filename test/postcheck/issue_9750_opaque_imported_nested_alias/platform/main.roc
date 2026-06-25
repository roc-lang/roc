platform ""
    requires {
        [Model : model] for program : {
            init : model,
        }
    }
    exposes [Types]
    packages {}
    provides { "roc_init_for_host": init_for_host }
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

import Types

init_for_host : () -> Model
init_for_host = || program.init
