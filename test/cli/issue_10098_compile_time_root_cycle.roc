# Repro for https://github.com/roc-lang/roc/issues/10098:
# a delayed reference from a function body must not create an eager
# compile-time dependency cycle.
lazy = |f| |x| f(x)(x)

a = lazy(|_| b)

b : U8 -> U8
b = a

main! = |_| {
    Ok({})
}
