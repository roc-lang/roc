platform "issue-10898"
    requires { program : {} -> {} }
    exposes []
    packages {}
    provides { "roc_main": main_for_host }
    hosted {}
    targets: {
        x64musl: { inputs: [app] },
    }

main_for_host : {} -> {}
main_for_host = program
