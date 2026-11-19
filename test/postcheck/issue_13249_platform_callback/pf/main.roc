platform ""
	requires {
		[Model : model] for main! : model => model
	}
	exposes []
	packages {}
	provides { "roc_main": main_for_host! }
	targets: {
		inputs_dir: "targets/",
		x64glibc: {
			inputs: [app],
			output: Archive,
		},
	}

import Io

send! : (U64, Model -> Model) => {}
send! = |reply| {
	write_reply! = |io, p| io.write_at!(|m| reply(p, m))
	write_reply!(Io.new(|_f| {}), 200)
}

main_for_host! : {} => {}
main_for_host! = |{}| send!(|_, m| m)
