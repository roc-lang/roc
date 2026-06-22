platform ""
    requires {} { process_string : Str -> Str }
    # INTENTIONALLY WRONG ORDER to test automatic dependency sorting!
    # Correct order would be: Utils, Core, Helper
    # Helper imports Core and Utils; Core imports Utils
    exposes [Helper, Core, Utils]
    packages {}
    provides { "roc_process_string": process_string_for_host }
    targets: {
        inputs_dir: "targets/",
        x64mac: { inputs: ["libhost.a", app] },
        arm64mac: { inputs: ["libhost.a", app] },
        x64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        x64glibc: { inputs: ["Scrt1.o", "crti.o", "libhost.a", app, "crtn.o", "libc.so"] },
        arm64glibc: { inputs: ["Scrt1.o", "crti.o", "libhost.a", app, "crtn.o", "libc.so"] },
        x64win: { inputs: ["host.lib", app] },
        arm64win: { inputs: ["host.lib", app] },
    }

import Core
import Helper
import Utils

process_string_for_host : Str -> Str
process_string_for_host = |input| {
    result = process_string(input)
    result
}

Simple := [A].{
    make : {} -> Simple
    make = |{}| A
}
