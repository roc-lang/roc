# repro for https://github.com/roc-lang/roc/issues/11944
#
# A closure captures the value a `var` holds where the closure is declared.
# Reassigning the `var` afterwards does not change what the closure sees, in
# every lowering strategy.
main! = |_args| {
    var $offset = 10
    var $name = "a"
    add_offset = |x| x + $offset
    greet = |s| Str.concat(s, $name)
    keep = |y| {
        _ = $name
        y
    }
    $offset = 20
    $name = "b"
    echo!("${add_offset(1).to_str()} ${greet("hi ")} ${keep("q")} ${keep(5).to_str()}\n")

    var $seen = []
    var $i = 0
    while $i < 3 {
        current = |_| $i
        $i = $i + 1
        $seen = $seen.append(current({}))
    }
    echo!("${Str.inspect($seen)}\n")
    Ok({})
}
