# `f` is generic: `t` carries only the interpolation requirement, so under
# `--specialize=no` the expect calls an erased callable whose argument
# descriptors travel in the invocation capture. `roc test --opt=dev` lowers the
# program with the hot-reload capture prefix, so the code generator running the
# expect must read those descriptors past the same prefix.
main! = |_args| Ok({})

f = |t| "x ${t}"

expect f("a") == "x a"
