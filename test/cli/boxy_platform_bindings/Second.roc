app [main!] { pf: platform "../../fx/platform/main.roc" }
import pf.Stdout
main! = || Stdout.line!("second")
