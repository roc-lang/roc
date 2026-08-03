app [State, program] { pf: platform "./platform/main.roc" }

import pf.Abi

State : U64

program = { make_machine!, }

from_state : U64 -> Abi.Machine
from_state = |remaining|
	Abi.Machine.Machine(
		Box.box(|wake|
			if remaining == 0 {
				Abi.Step.End
			} else {
				first = from_state(remaining - 1)
				second = from_state(remaining - 1)
				if wake == 0 {
					Abi.Step.Emit({ machine: first, observed: remaining })
				} else {
					Abi.Step.Emit({ machine: second, observed: remaining + wake })
				}
			}),
	)

make_machine! : U64, State => Abi.Machine
make_machine! = |remaining, _state| from_state(remaining)
