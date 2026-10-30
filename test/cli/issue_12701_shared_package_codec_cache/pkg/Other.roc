import Api

Other := [].{
	other : Str -> Try([Invalid], [InvalidJson(Str)])
	other = Api.parser({})
}
