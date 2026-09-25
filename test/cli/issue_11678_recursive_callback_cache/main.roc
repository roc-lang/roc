app [main!] { pf: platform "../../fx-open/platform/main.roc" }
import pf.Stdout
import Model
program = Model.make(Model.Handle.({}))

main! = |args| {
	Stdout.line!(program(args.len() - 1))
	Ok({})
}
