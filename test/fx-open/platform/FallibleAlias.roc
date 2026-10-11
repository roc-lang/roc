import FallibleHostAlias

# Reconstruct an alias-wrapped hosted result into a wider Roc-owned row.
FallibleAlias := [].{
	via_question_closed_wider! : {} => Try(Str, [HostErr(Str), Widened(I32)])
	via_question_closed_wider! = |{}|
		match FallibleHostAlias.str_ok!({}) {
			Ok(value) => Ok(value)
			Err(HostErr(message)) => Err(HostErr(message))
		}
}
