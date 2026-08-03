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
				next = from_state(remaining - 1)
				forwarded = next
				Abi.Step.Emit({ machine: forwarded, observed: remaining + wake })
			}),
	)

make_machine! : U64, State => Abi.Machine
make_machine! = |remaining, _state| from_state(remaining)
