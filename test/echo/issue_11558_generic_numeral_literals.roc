# repro for https://github.com/roc-lang/roc/issues/11558
#
# A numeric literal in a generic body takes the type its caller instantiates,
# not the Dec default: here every use is `I64`. Every backend, including
# `--specialize=no`, prints the same results.
add5 = |n| n + 5

app_main = { init: |{}| 5 }

main! = |args| {
    x : I64
    x = add5(List.len(args).to_i64_wrap())
    y : I64
    y = (app_main.init)({})
    echo!("${Str.inspect(x)} ${Str.inspect(y)}\n")
    Ok({})
}
