platform ""
    requires {} { process_string : Str -> Str }
    exposes [Helper, Core]
    packages {}
    provides { process_string_for_host: "process_string" }
    targets: {
        files: "targets/",
        exe: {
            x64mac: ["libhost.a", app],
            arm64mac: ["libhost.a", app],
            x64musl: ["crt1.o", "libhost.a", app, "libc.a"],
            arm64musl: ["crt1.o", "libhost.a", app, "libc.a"],
            x64glibc: ["Scrt1.o", "crti.o", "libhost.a", app, "crtn.o", "libc.so"],
            arm64glibc: ["Scrt1.o", "crti.o", "libhost.a", app, "crtn.o", "libc.so"],
        }
    }

import Core
import Helper

process_string_for_host : Str -> Str
process_string_for_host = process_string
