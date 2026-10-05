# repro for https://github.com/roc-lang/roc/issues/11849
#
# An interpolation whose target type is generic keeps its
# `from_interpolation` dispatch, and the selected method fixes the type of the
# generated iterator's items.
greet = |name| "Hello, ${name}"
pair = |a, b| "${a} and ${b}!"
wrap = |s| "[${greet(s)}]"
twice = |f, x| "${f(x)}${f(x)}"

main! = |_| {
    echo!(greet("Ada"))
    echo!(pair("x", "y"))
    echo!(wrap("Bo"))
    echo!(twice(|s| s.concat("!"), "hi"))
    shout = |s| "${s}${s}"
    echo!(shout("ab"))
    echo!("n=${5.I64.to_str()} ${greet("Cy")}")
    Ok({})
}
