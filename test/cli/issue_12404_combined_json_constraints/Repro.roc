# repro for https://github.com/roc-lang/roc/issues/12404
platform ""
    requires { seed : {} -> {} }
    exposes []
    packages {}
    provides { "repro": repro! }
import Definition

bind : a -> {} where [a.Json.Encodable(encode_errs), a.Json.Parseable([InvalidJson(Str), ..errs])]
bind = |script| {
    _ = Definition.component("", script)
    {}
}
repro! = |_| {
    _ = bind({ value: 3.5.F64 })
    _ = bind({ notes: [""] })
    {}
}
