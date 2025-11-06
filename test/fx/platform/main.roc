platform ""
    requires {} {
        main! : () => {},
        putStdout! : Str => {},
        putStderr! : Str => {}
    }
    exposes []
    packages {}
    provides {
        main_for_host: "main_for_host",
        putStdout: "putStdout",
        putStderr: "putStderr"
    }

main_for_host! : () => {}
main_for_host! = || main!()

putStdout! : Str => {}
putStderr! : Str => {}
