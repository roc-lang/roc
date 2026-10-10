import FallibleHost

# Rejected counterpart of platform/FallibleChannels.roc: the same non-`?`
# channels, each asking for an error row wider than the one FallibleHost.str_ok!
# declares. A hosted function's declared row is its host ABI (design.md "Host
# Symbol ABI"). Forwarding cannot widen it; callers must reconstruct errors.
FallibleWiden := [].{
	# Channel: an annotated binding at a wider row.
	via_annotation! : {} => Try(Str, [HostErr(Str), Widened(I32)])
	via_annotation! = |{}| {
		value : Try(Str, [HostErr(Str), Widened(I32)])
		value = FallibleHost.str_ok!({})
		value
	}

	# Channel: an argument position whose parameter row is wider.
	via_argument! : {} => Try(Str, [HostErr(Str), Widened(I32)])
	via_argument! = |{}| widen(FallibleHost.str_ok!({}))

	widen : Try(Str, [HostErr(Str), Widened(I32)]) -> Try(Str, [HostErr(Str), Widened(I32)])
	widen = |value| value

	# Channel: a record field at a wider row.
	via_record_field! : {} => Try(Str, [HostErr(Str), Widened(I32)])
	via_record_field! = |{}| {
		holder : { result : Try(Str, [HostErr(Str), Widened(I32)]) }
		holder = { result: FallibleHost.str_ok!({}) }
		holder.result
	}
}
