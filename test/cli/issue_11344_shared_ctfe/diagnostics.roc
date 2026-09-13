import Diagnostics

main! = |_args| {
    _ = Diagnostics.observed
    _ = Diagnostics.failed
    _ = Diagnostics.crashed
    Ok({})
}
