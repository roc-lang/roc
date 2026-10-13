app [main!] { pf: platform "../../fx-open/platform/main.roc" }

# The imported default binds a value before its lambda, so lowering it relates
# a binder of the declaring module.
import Suffixed

main! = |_args| {
    _ = Suffixed.{}
    _ = |x| x
    Ok({})
}
