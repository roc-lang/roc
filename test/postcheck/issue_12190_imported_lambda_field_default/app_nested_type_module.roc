app [main!] { pf: platform "../../fx-open/platform/main.roc" }

# The imported default is declared in a type nested inside another type module.
import Vcr

main! = |_args| {
    _ = Vcr.Config.{}
    _ = |x| x
    Ok({})
}
