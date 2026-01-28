platform ""
    requires {
        make_glue : List(Types) -> Try(List(File), Str)
    }
    exposes [File, Types]
    packages {}
    provides { make_glue_for_host: "make_glue" }
    targets: {
        files: "targets/",
        exe: {
            x64mac: ["libhost.a", app],
            arm64mac: ["libhost.a", app],
            x64musl: ["crt1.o", "libhost.a", app, "libc.a"],
            arm64musl: ["crt1.o", "libhost.a", app, "libc.a"],
            x64win: ["host.lib", app],
            arm64win: ["host.lib", app],
        }
    }

import Types exposing [Types]
import File exposing [File]

make_glue_for_host : List(Types) -> Try(List(File), Str)
make_glue_for_host = make_glue
