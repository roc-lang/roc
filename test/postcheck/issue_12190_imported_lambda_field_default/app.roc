app [main!] { pf: platform "../../fx-open/platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/12190
# Omitting an imported nominal record's lambda field default, then writing a
# later lambda in the same body, must lower through Monotype.
import Config

main! = |_args| {
    _ = Config.{}
    _ = |x| x
    Ok({})
}
