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
        x64v1musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        arm64v1musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        x64glibc: { inputs: ["Scrt1.o", "crti.o", "libhost.a", app, "crtn.o", "libc.so"] },
        arm64glibc: { inputs: ["Scrt1.o", "crti.o", "libhost.a", app, "crtn.o", "libc.so"] },
        x64win: { inputs: ["host.lib", app] },
        arm64win: { inputs: ["host.lib", app] },
        x64mingw: { inputs: ["crt2.obj", "host.lib", app, "libmingw32.lib", "zigc.lib", "compiler_rt.lib", "api-ms-win-crt-conio-l1-1-0.lib", "api-ms-win-crt-convert-l1-1-0.lib", "api-ms-win-crt-environment-l1-1-0.lib", "api-ms-win-crt-filesystem-l1-1-0.lib", "api-ms-win-crt-heap-l1-1-0.lib", "api-ms-win-crt-locale-l1-1-0.lib", "api-ms-win-crt-math-l1-1-0.lib", "api-ms-win-crt-multibyte-l1-1-0.lib", "api-ms-win-crt-private-l1-1-0.lib", "api-ms-win-crt-process-l1-1-0.lib", "api-ms-win-crt-runtime-l1-1-0.lib", "api-ms-win-crt-stdio-l1-1-0.lib", "api-ms-win-crt-string-l1-1-0.lib", "api-ms-win-crt-time-l1-1-0.lib", "api-ms-win-crt-utility-l1-1-0.lib", "advapi32.lib", "kernel32.lib", "ntdll.lib", "shell32.lib", "user32.lib"] },
        arm64mingw: { inputs: ["crt2.obj", "host.lib", app, "libmingw32.lib", "zigc.lib", "compiler_rt.lib", "api-ms-win-crt-conio-l1-1-0.lib", "api-ms-win-crt-convert-l1-1-0.lib", "api-ms-win-crt-environment-l1-1-0.lib", "api-ms-win-crt-filesystem-l1-1-0.lib", "api-ms-win-crt-heap-l1-1-0.lib", "api-ms-win-crt-locale-l1-1-0.lib", "api-ms-win-crt-math-l1-1-0.lib", "api-ms-win-crt-multibyte-l1-1-0.lib", "api-ms-win-crt-private-l1-1-0.lib", "api-ms-win-crt-process-l1-1-0.lib", "api-ms-win-crt-runtime-l1-1-0.lib", "api-ms-win-crt-stdio-l1-1-0.lib", "api-ms-win-crt-string-l1-1-0.lib", "api-ms-win-crt-time-l1-1-0.lib", "api-ms-win-crt-utility-l1-1-0.lib", "advapi32.lib", "kernel32.lib", "ntdll.lib", "shell32.lib", "user32.lib"] },
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
