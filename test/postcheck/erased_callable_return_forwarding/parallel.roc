app [State, program] { pf: platform "./platform/main.roc" }

import pf.Abi

State : U64

program = { make_machine!, }

from_first_state : U64 -> Abi.Machine
from_first_state = |remaining|
	Abi.Machine.Machine(
		Box.box(|wake|
			if remaining == 0 {
				Abi.Step.End
			} else {
				next = from_first_state(remaining - 1)
				Abi.Step.Emit({ machine: next, observed: remaining + wake })
			}),
	)

from_second_state : U64 -> Abi.Machine
from_second_state = |remaining|
	Abi.Machine.Machine(
		Box.box(|wake|
			if remaining == 0 {
				Abi.Step.End
			} else {
				next = from_second_state(remaining - 1)
				Abi.Step.Emit({ machine: next, observed: remaining + wake })
			}),
	)

make_machine! : U64, State => Abi.Machine
make_machine! = |remaining, _state|
	if remaining == 0
		from_first_state(remaining)
	else
		from_second_state(remaining)
