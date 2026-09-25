platform ""
    requires { [State : state] for main : Program(state) }
    exposes [Program]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {}
    targets: {
        inputs_dir: "targets/",
        arm64mac: { inputs: [app], output: Archive },
        x64mac: { inputs: [app], output: Archive },
    }

import Program

main_for_host! : {} => {}
main_for_host! = |_| {}
