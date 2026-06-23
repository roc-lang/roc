platform ""
    requires {} { main! : List(Str) => Try({}, [Exit(I32), ..]) }
    exposes [Stdout, Stderr, Stdin]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_stderr_line": Stderr.line!,
        "roc_stdin_line": Stdin.line!,
        "roc_stdout_line": Stdout.line!,
    }
    targets: {
        inputs_dir: "targets/",
        x64mac: { inputs: ["libhost.a", app] },
        arm64mac: { inputs: ["libhost.a", app] },
        x64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        x64win: { inputs: ["host.lib", app] },
        arm64win: { inputs: ["host.lib", app] },
    }

import Stdout
import Stderr
import Stdin

main_for_host! : List(Str) => I32
main_for_host! = |args|
    match main!(args) {
        Ok({}) => 0
        Err(Exit(code)) => code
        Err(other) => {
            Stderr.line!("exited with other error: ${Str.inspect(other)}")
            1
        }
    }
