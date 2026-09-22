# The crash happens in this app's own method while evaluating a top-level
# constant at compile time, so the report must point at this file. Module
# indices are only unique within one package, and this app's index collides
# with a platform module's.
Ratio := { n : I64 }.{
    div_by : Ratio, Ratio -> Ratio
    div_by = |a, b| {
        if b.n == 0 {
            crash "Ratio division by zero"
        } else {
            { n: a.n // b.n }
        }
    }
}

bad : Ratio
bad = Ratio.{ n: 2 } / Ratio.{ n: 0 }

main! = |_args| {
    echo!(bad.n.to_str())
    Ok({})
}
