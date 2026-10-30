import Api

Form := [].{
	submit : Str -> Try([Invalid], [InvalidJson(Str)])
	submit = Api.parser({})
}
