platform ""
    requires {
        [Foo : foo] for bar : {} -> Foo,
        baz : Foo -> {},
    }
    exposes []
    packages {}
    provides { "roc_bar": bar_for_host, "roc_baz": baz_for_host }
    targets: {
        inputs_dir: "targets/",
        wasm32: { inputs: ["host.wasm", app] },
    }

bar_for_host : {} -> Box(Foo)
bar_for_host = |{}| Box.box(bar({}))

baz_for_host : Box(Foo) -> {}
baz_for_host = |b| baz(Box.unbox(b))
