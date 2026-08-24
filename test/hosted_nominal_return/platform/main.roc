platform ""
    requires {} { main! : () => {} }
    exposes [Effect]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_bar_idx_get": Bar.Idx.get!,
        "roc_effect_things": Effect.things!,
        "roc_foo_idx_get": Foo.Idx.get!,
    }
    targets: {
        inputs_dir: "../../fx/platform/targets/",
        arm64mac: { inputs: ["libhost.a", app] },
        arm64musl: { inputs: ["libhost.a", app] },
        arm64v1musl: { inputs: ["libhost.a", app] },
        x64musl: { inputs: ["libhost.a", app] },
        x64v1musl: { inputs: ["libhost.a", app] },
        arm64win: { inputs: ["host.lib", app] },
        x64win: { inputs: ["host.lib", app] },
        x64mingw: { inputs: ["crt2.obj", "host.lib", app, "libmingw32.lib", "zigc.lib", "compiler_rt.lib", "api-ms-win-crt-conio-l1-1-0.lib", "api-ms-win-crt-convert-l1-1-0.lib", "api-ms-win-crt-environment-l1-1-0.lib", "api-ms-win-crt-filesystem-l1-1-0.lib", "api-ms-win-crt-heap-l1-1-0.lib", "api-ms-win-crt-locale-l1-1-0.lib", "api-ms-win-crt-math-l1-1-0.lib", "api-ms-win-crt-multibyte-l1-1-0.lib", "api-ms-win-crt-private-l1-1-0.lib", "api-ms-win-crt-process-l1-1-0.lib", "api-ms-win-crt-runtime-l1-1-0.lib", "api-ms-win-crt-stdio-l1-1-0.lib", "api-ms-win-crt-string-l1-1-0.lib", "api-ms-win-crt-time-l1-1-0.lib", "api-ms-win-crt-utility-l1-1-0.lib", "advapi32.lib", "kernel32.lib", "ntdll.lib", "shell32.lib", "user32.lib"] },
        arm64mingw: { inputs: ["crt2.obj", "host.lib", app, "libmingw32.lib", "zigc.lib", "compiler_rt.lib", "api-ms-win-crt-conio-l1-1-0.lib", "api-ms-win-crt-convert-l1-1-0.lib", "api-ms-win-crt-environment-l1-1-0.lib", "api-ms-win-crt-filesystem-l1-1-0.lib", "api-ms-win-crt-heap-l1-1-0.lib", "api-ms-win-crt-locale-l1-1-0.lib", "api-ms-win-crt-math-l1-1-0.lib", "api-ms-win-crt-multibyte-l1-1-0.lib", "api-ms-win-crt-private-l1-1-0.lib", "api-ms-win-crt-process-l1-1-0.lib", "api-ms-win-crt-runtime-l1-1-0.lib", "api-ms-win-crt-stdio-l1-1-0.lib", "api-ms-win-crt-string-l1-1-0.lib", "api-ms-win-crt-time-l1-1-0.lib", "api-ms-win-crt-utility-l1-1-0.lib", "advapi32.lib", "kernel32.lib", "ntdll.lib", "shell32.lib", "user32.lib"] },
    }

import Effect

main_for_host! : () => {}
main_for_host! = main!
