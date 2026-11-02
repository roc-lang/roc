Api := [].{
	Endpoint(request, answer, error) :: {
		path : Str,
		status : error -> U16,
		parse_error : Str -> Try(error, [NotAnError]),
	}

	Failure : [Timeout]

	created : Str, (error -> U16) -> Api.Endpoint(request, answer, error) where [error.Json.Parseable([InvalidJson(Str), ..errs])]
	created = |path, status| {
		path,
		status,
		parse_error: |text| Json.parse(text).map_err(|_| NotAnError),
	}

	status_of : Api.Endpoint(request, answer, error), error -> U16
	status_of = |e, error| (e.status)(error)

	Secret := { n : U16 }

	make_adder : {} -> (U16 -> U16)
	make_adder = |{}| {
		s : Api.Secret
		s = { n: 1 }
		|x| x + s.n
	}
}
