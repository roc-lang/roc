# A top-level constant record whose field type is a transparent alias of a
# function type, holding a lambda literal, used to panic monotype lowering:
#   postcheck invariant violated: checked callable relation received a
#   non-function request node   (src/postcheck/monotype/lower.zig)
# The request node kept the alias wrapper while the relation demanded a
# structural function; callable requests now resolve to the structural root.

ReadFn(state) : state => state

Ops(state) : {
    read! : ReadFn(state),
    scale : U64,
}

ops : Ops(U64)
ops = {
    read!: |n| n + 1,
    scale: 2,
}

main! = |_args| {
    n = (ops.read!)(20)
    echo!(if n * ops.scale == 42 "ok" else "bad")
    Ok({})
}
