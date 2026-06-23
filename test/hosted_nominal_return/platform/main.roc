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
        x64musl: { inputs: ["libhost.a", app] },
        arm64win: { inputs: ["host.lib", app] },
        x64win: { inputs: ["host.lib", app] },
    }

import Effect

main_for_host! : () => {}
main_for_host! = main!
