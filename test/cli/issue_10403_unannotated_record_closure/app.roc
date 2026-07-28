app [program] { pf: platform "./platform/main.roc" }

program = { run: run }

run : Str -> Str
run = |_s| {
	_io = {
		write: |bytes| bytes.len() + 1,
	}
	"ok"
}
