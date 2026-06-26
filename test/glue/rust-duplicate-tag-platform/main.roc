platform ""
    requires {
        main! : () => {}
    }
    exposes [IOErr, Host, A, B, C, D]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_fallible_unit": Host.fallible_unit!,
        "roc_fallible_str": Host.fallible_str!,
        "roc_fallible_bytes": Host.fallible_bytes!,
        "roc_fallible_record": Host.fallible_record!,
        "roc_fallible_nested": Host.fallible_nested!,
        "roc_a_unit": A.unit!,
        "roc_a_str": A.str!,
        "roc_a_bytes": A.bytes!,
        "roc_a_record": A.record!,
        "roc_a_nested": A.nested!,
        "roc_b_unit": B.unit!,
        "roc_b_str": B.str!,
        "roc_b_bytes": B.bytes!,
        "roc_b_record": B.record!,
        "roc_b_nested": B.nested!,
        "roc_c_unit": C.unit!,
        "roc_c_str": C.str!,
        "roc_c_bytes": C.bytes!,
        "roc_c_record": C.record!,
        "roc_c_nested": C.nested!,
        "roc_d_unit": D.unit!,
        "roc_d_str": D.str!,
        "roc_d_bytes": D.bytes!,
        "roc_d_record": D.record!,
        "roc_d_nested": D.nested!,
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

import IOErr
import Host
import A
import B
import C
import D

main_for_host! : () => {}
main_for_host! = main!
