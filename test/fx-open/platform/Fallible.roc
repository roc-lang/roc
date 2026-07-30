import FallibleHost

Fallible := [].{
	via_question! : {} => Try(Str, [HostErr(Str), ..])
	via_question! = |{}| Ok(FallibleHost.str_ok!({})?)

	via_match! : {} => Try(Str, [HostErr(Str), ..])
	via_match! = |{}|
		match FallibleHost.str_ok!({}) {
			Ok(value) => Ok(value)
			Err(HostErr(msg)) => Err(HostErr(msg))
		}
}
