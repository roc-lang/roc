app [main!] { pf: platform "./platform/main.roc" }

# The app names `Arg` only through the platform's requirement: it imports no
# platform module, and inspecting its arguments calls `Arg.to_inspect`.
shown = |args| Str.inspect(args)

main! = |args|
	if shown(args) == "[]" {
		Ok({})
	} else {
		Err(Exit(1))
	}

expect shown([]) == "[]"
