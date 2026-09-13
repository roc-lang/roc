app [main!] { pf: platform "../../fx/platform/main.roc", custom0: "custom0/main.roc", custom1: "custom1/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/11065: both packages ship
# identical Plugin modules, so a warm check loads the same cached module twice.
import custom0.Plugin as Custom0
import custom1.Plugin as Custom1
import pf.Stdout

main! = || Stdout.line!("${Custom0.name()} ${Custom1.name()}")
