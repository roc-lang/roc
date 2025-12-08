platform ""
    requires {} { main! : List(Str) => Try({}, [Exit(I32), ..others]) }
    exposes [Stdout, Stderr, Stdin]
    packages {}
    provides { main_for_host!: "main" }
    targets: {
        exe: {
            x64mac: [app],
            arm64mac: [app],
            x64musl: [app],
            arm64musl: [app],
        }
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
            Stderr.line!("exited with other error: ${inspect other})
            1
        }
    }
