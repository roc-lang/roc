platform ""
    requires { main! : U64 => {} }
    exposes []
    packages {}
    provides { "entry": start! }
    targets: {
        x64glibc: { inputs: [app], output: Archive },
        arm64glibc: { inputs: [app], output: Archive },
        x64musl: { inputs: [app], output: Archive },
        arm64musl: { inputs: [app], output: Archive },
        x64mac: { inputs: [app], output: Archive },
        arm64mac: { inputs: [app], output: Archive },
        x64win: { inputs: [app], output: Archive },
        arm64win: { inputs: [app], output: Archive },
        x64mingw: { inputs: [app], output: Archive },
        arm64mingw: { inputs: [app], output: Archive },
    }

value = List.len([1, 2])
start! = || {
    main!(value)
}
