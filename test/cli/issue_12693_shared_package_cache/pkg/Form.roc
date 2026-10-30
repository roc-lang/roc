import Api

Form := [].{

	SubmitError : [Invalid(Str), Server(Api.Failure)]

	submit : Api.Endpoint({}, {}, SubmitError)
	submit = Api.created("/submit", |_| 400)
}
