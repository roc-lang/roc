import FallibleHost
import FallibleNotFound

# Every channel a hosted function's result reaches a caller through, each
# asking for an error row wider than the one FallibleHost.str_ok! declares. A
# hosted function's `Try` error row is closed by declaration, and row
# subsumption re-opens it at every use exactly as it re-opens a forwarding Roc
# function's (design.md "Row Subsumption"), so each channel typechecks.
# Lowering serves every widened use with an adapter that still calls the
# extern at its declared row (design.md "Host Symbol ABI"), so the host's
# Ok("ok") arrives as Ok on every line of hosted_widening_channels.roc.
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

	# Channel: the hosted function carried as a value before it is called.
	via_value! : {} => Try(Str, [HostErr(Str), Widened(I32)])
	via_value! = |{}| {
		run! = FallibleHost.str_ok!
		value : Try(Str, [HostErr(Str), Widened(I32)])
		value = run!({})
		value
	}

	# Channel: the hosted function passed to a higher-order function whose
	# parameter type names the wider row.
	via_higher_order! : {} => Try(Str, [HostErr(Str), Widened(I32)])
	via_higher_order! = |{}| call_wide!(FallibleHost.str_ok!)

	call_wide! : ({} => Try(Str, [HostErr(Str), Widened(I32)])) => Try(Str, [HostErr(Str), Widened(I32)])
	call_wide! = |run!| run!({})

	# Channel: the hosted function boxed at the wider function type, then
	# unboxed and called.
	via_box! : {} => Try(Str, [HostErr(Str), Widened(I32)])
	via_box! = |{}| {
		boxed : Box(({} => Try(Str, [HostErr(Str), Widened(I32)])))
		boxed = Box.box(FallibleHost.str_ok!)
		run! = Box.unbox(boxed)
		run!({})
	}

	# Channel: `?` inside a function with no annotation, used at a wider row.
	via_unannotated_question! : {} => Try(Str, [HostErr(Str), Widened(I32)])
	via_unannotated_question! = |{}| unannotated_question!({})

	unannotated_question! = |{}| Ok(FallibleHost.str_ok!({})?)

	# A host that actually returns Err. FallibleNotFound.not_found! returns
	# Err(NotFound) from its declared row [NotFound, PermissionDenied]; asked
	# for here at a row where `Aborted` sorts first, every declared
	# discriminant shifts by one, so a missing or misordered re-tag reads a
	# different tag than the host returned.
	via_host_err! : {} => Try(Str, [Aborted, NotFound, PermissionDenied])
	via_host_err! = |{}| {
		value : Try(Str, [Aborted, NotFound, PermissionDenied])
		value = FallibleNotFound.not_found!({})
		value
	}

	# Channel: the hosted function named through an alias of its owner.
	via_alias_owner! : {} => Try(Str, [HostErr(Str), Widened(I32)])
	via_alias_owner! = |{}| {
		value : Try(Str, [HostErr(Str), Widened(I32)])
		value = HostAlias.str_ok!({})
		value
	}
}

HostAlias : FallibleHost
