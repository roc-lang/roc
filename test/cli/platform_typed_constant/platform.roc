platform ""
    requires { [Model : model] for program : { init : () -> model, count : model -> U64 } }
    exposes []
    packages {}
    provides { "roc_run": run_for_host }
    targets: {
        x64musl: { inputs: [app], output: Archive },
    }

# A constant whose type mentions the platform's abstract type: its value is
# computed where the platform is paired with the app, at the app's `Model`.
models : List(Model)
models = List.with_capacity(3)

run_for_host : U64 -> U64
run_for_host = |n| {
    ms = List.append(models, (program.init)())
    n + List.len(ms) + List.fold(ms, 0, |acc, m| acc + (program.count)(m))
}
