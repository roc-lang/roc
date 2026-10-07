app [main!] { pf: platform "../../fx-open/platform/main.roc" }
import pf.Stdout
import Model
program = Model.wait_next

main! = |args| {
	Stdout.line!(program(args.len() - 1, Model.Handle.({})))
	Ok({})
}
