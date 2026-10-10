import FallibleHost

# Non-`?` channels a hosted function's result flows through, each at the
# declared error row. FallibleHost.str_ok! is declared Try(Str, [HostErr(Str)])
# and its host always returns Ok("ok"), so every channel here must deliver that
# Ok: the extern stays at the declared row (design.md "Host Symbol ABI"), which
# Monotype lowering enforces while building this module.
# platform/FallibleWiden.roc is the rejected counterpart, where the same
# channels ask for a row wider than the declared one.
FallibleChannels := [].{
	# Channel: an annotated binding.
	via_annotation! : {} => Try(Str, [HostErr(Str)])
	via_annotation! = |{}| {
		value : Try(Str, [HostErr(Str)])
		value = FallibleHost.str_ok!({})
		value
	}

	# Channel: an argument position.
	via_argument! : {} => Try(Str, [HostErr(Str)])
	via_argument! = |{}| pass_through(FallibleHost.str_ok!({}))

	pass_through : Try(Str, [HostErr(Str)]) -> Try(Str, [HostErr(Str)])
	pass_through = |value| value

	# Channel: a record field.
	via_record_field! : {} => Try(Str, [HostErr(Str)])
	via_record_field! = |{}| {
		holder : { result : Try(Str, [HostErr(Str)]) }
		holder = { result: FallibleHost.str_ok!({}) }
		holder.result
	}

	# Channel: `?` into a closed row wider than the declared one. The Hosted
	# Try Question Widening rule accepts this the same way it accepts an open
	# enclosing row, and lowering bridges it with an adapter, so the boundary
	# is still called at the declared row.
	via_question_closed_wider! : {} => Try(Str, [HostErr(Str), Widened(I32)])
	via_question_closed_wider! = |{}| Ok(FallibleHost.str_ok!({})?)

	# A caller that wants a wider row re-tags the hosted error itself. That is
	# the caller's own value, so the boundary keeps its declared row.
	via_retag! : {} => Try(Str, [HostErr(Str), Widened(I32)])
	via_retag! = |{}|
		match FallibleHost.str_ok!({}) {
			Ok(value) => Ok(value)
			Err(HostErr(message)) => Err(HostErr(message))
		}
}
