platform ""
    requires {
        main! : I64 => I64
    }
    exposes [Host]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_host_double": Host.nonexistent!,
        "roc-host-bad": Host.double!,
        "roc_alloc": Host.double!,
        "roc__sneaky": Host.double!,
        "roc_main": Host.triple!,
    }
    targets: {
        inputs_dir: "targets/",
        arm64mac: { inputs: [app] },
        x64mac: { inputs: [app] },
        x64glibc: { inputs: [app] },
        arm64glibc: { inputs: [app] },
        x64musl: { inputs: [app] },
        x64v1musl: { inputs: [app] },
        arm64musl: { inputs: [app] },
        arm64v1musl: { inputs: [app] },
        x64win: { inputs: [app] },
        arm64win: { inputs: [app] },
        x64mingw: { inputs: ["crt2.obj", app, "libmingw32.lib", "zigc.lib", "compiler_rt.lib", "api-ms-win-crt-conio-l1-1-0.lib", "api-ms-win-crt-convert-l1-1-0.lib", "api-ms-win-crt-environment-l1-1-0.lib", "api-ms-win-crt-filesystem-l1-1-0.lib", "api-ms-win-crt-heap-l1-1-0.lib", "api-ms-win-crt-locale-l1-1-0.lib", "api-ms-win-crt-math-l1-1-0.lib", "api-ms-win-crt-multibyte-l1-1-0.lib", "api-ms-win-crt-private-l1-1-0.lib", "api-ms-win-crt-process-l1-1-0.lib", "api-ms-win-crt-runtime-l1-1-0.lib", "api-ms-win-crt-stdio-l1-1-0.lib", "api-ms-win-crt-string-l1-1-0.lib", "api-ms-win-crt-time-l1-1-0.lib", "api-ms-win-crt-utility-l1-1-0.lib", "advapi32.lib", "kernel32.lib", "ntdll.lib", "shell32.lib", "user32.lib"] },
        arm64mingw: { inputs: ["crt2.obj", app, "libmingw32.lib", "zigc.lib", "compiler_rt.lib", "api-ms-win-crt-conio-l1-1-0.lib", "api-ms-win-crt-convert-l1-1-0.lib", "api-ms-win-crt-environment-l1-1-0.lib", "api-ms-win-crt-filesystem-l1-1-0.lib", "api-ms-win-crt-heap-l1-1-0.lib", "api-ms-win-crt-locale-l1-1-0.lib", "api-ms-win-crt-math-l1-1-0.lib", "api-ms-win-crt-multibyte-l1-1-0.lib", "api-ms-win-crt-private-l1-1-0.lib", "api-ms-win-crt-process-l1-1-0.lib", "api-ms-win-crt-runtime-l1-1-0.lib", "api-ms-win-crt-stdio-l1-1-0.lib", "api-ms-win-crt-string-l1-1-0.lib", "api-ms-win-crt-time-l1-1-0.lib", "api-ms-win-crt-utility-l1-1-0.lib", "advapi32.lib", "kernel32.lib", "ntdll.lib", "shell32.lib", "user32.lib"] },
    }

import Host

main_for_host! : I64 => I64
main_for_host! = |n| main!(n)
