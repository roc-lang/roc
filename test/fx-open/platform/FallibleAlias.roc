import FallibleHostAlias

# The alias-wrapped twin of platform/FallibleChannels.roc's
# via_question_closed_wider!: `?` on a direct hosted call whose declared result
# is an alias over `Try`, into a closed row wider than the declared one. The
# Hosted Try Question Widening rule bridges it with a generated adapter, so the
# boundary is still called at the declared row and the host's Ok("ok") arrives
# as Ok.
FallibleAlias := [].{
	via_question_closed_wider! : {} => Try(Str, [HostErr(Str), Widened(I32)])
	via_question_closed_wider! = |{}| Ok(FallibleHostAlias.str_ok!({})?)
}
