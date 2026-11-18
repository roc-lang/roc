app [main!] { pf: platform "../../fx/platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/13043
# Two fields of a borrowed tag payload must be valid arguments to one call.
import pf.Stdout

Node : [Op({ args : List(Str), ty : [Leaf, Named(Str, List(Str))] }), Param]

render : Node -> Str
render = |node| match node {
    Op(op) => pick(op.args, op.ty)
    Param => "param"
}

pick : List(Str), [Leaf, Named(Str, List(Str))] -> Str
pick = |args, ty| if args.is_empty() name(ty) else "args"

name : [Leaf, Named(Str, List(Str))] -> Str
name = |ty| match ty {
    Leaf => "leaf"
    Named(n, parts) => if parts.is_empty() n else Str.join_with(parts, ", ")
}

main! = || {
    Stdout.line!(render(Op({ args: ["a"], ty: Leaf })))
}

expect render(Op({ args: ["a"], ty: Leaf })) == "args"
expect render(Op({ args: [], ty: Leaf })) == "leaf"
expect render(Op({ args: [], ty: Named("n", []) })) == "n"
expect render(Op({ args: [], ty: Named("n", ["a", "b"]) })) == "a, b"
expect render(Param) == "param"
