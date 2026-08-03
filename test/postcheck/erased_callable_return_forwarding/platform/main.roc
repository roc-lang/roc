platform "erased-callable-return-forwarding"
	requires {
		[State : state] for program : {
			make_machine! : U64, state => Abi.Machine,
		}
	}
	exposes [Abi]
	packages {}
	provides {
		"roc_make_machine": make_machine_for_host!,
		"roc_advance_machine": advance_machine_for_host!,
	}
	targets: {}

import Abi

make_machine_for_host! : U64, State => Abi.Machine
make_machine_for_host! = program.make_machine!

advance_machine_for_host! : Abi.Machine, U64 => Abi.Step
advance_machine_for_host! = |machine, wake|
	match machine {
		Abi.Machine.Machine(boxed) => (Box.unbox(boxed))(wake)
	}
