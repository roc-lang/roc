import Api

Form := [].{

	SubmitError : [Invalid(Str), Server(Api.Failure)]

	submit : Api.Endpoint({}, {}, SubmitError)
	submit = Api.created("/submit", |_| 400)

	adder : U16 -> U16
	adder = Api.make_adder({})
}
