platform ""
	requires {
		[State : state] for main : Prog(state)
	}
	exposes [Prog]
	packages {}
	provides { "roc_init": init!, "roc_reload": reload! }
	hosted { "roc_set_dispatch": Host.set_dispatch! }
	targets: {
		inputs_dir: "targets/",
		x64mac: { inputs: [app], output: Archive },
		arm64mac: { inputs: [app], output: Archive },
		x64musl: { inputs: [app], output: Archive },
		x64v1musl: { inputs: [app], output: Archive },
		arm64musl: { inputs: [app], output: Archive },
		arm64v1musl: { inputs: [app], output: Archive },
		x64glibc: { inputs: [app], output: Archive },
		arm64glibc: { inputs: [app], output: Archive },
		x64win: { inputs: [app], output: Archive },
		arm64win: { inputs: [app], output: Archive },
	}

import Prog
import Host
import Session

init! : () => {}
init! = || Prog.start!(main)

reload! : Box((Session(State) => {})) => {}
reload! = |dispatch_box| Box.unbox(dispatch_box)(Session.reload(Prog.renderer(main)))
