# The `+` on `Str` is a type error, so each loop body below is a checked
# runtime error. Loops that never run their body finish normally; the last
# loop runs its body and crashes.
main! = |_args| {
    for n in List.drop_first([1.U32], 1) {
        echo!(n.to_str() + "\n")
    }

    var $skipped = 0
    while $skipped > 0 {
        $skipped = $skipped - 1
        n = 5.U32
        echo!(n.to_str() + "\n")
    }

    echo!("loops skipped")

    var $count = 0
    while $count < 3 {
        $count = $count + 1
        n = 5.U32
        echo!(n.to_str() + "\n")
    }

    Ok({})
}
