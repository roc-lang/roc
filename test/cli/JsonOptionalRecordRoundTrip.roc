JsonOptionalRecordRoundTrip :: [].{}

Shape : {
	f00 : Try(Str, [Missing]),
	f01 : Try(Str, [Missing]),
	f02 : Try(Str, [Missing]),
	f03 : Try(Str, [Missing]),
	f04 : Try(Str, [Missing]),
	f05 : Try(Str, [Missing]),
	f06 : Try(Str, [Missing]),
	f07 : Try(Str, [Missing]),
	f08 : Try(Str, [Missing]),
	f09 : Try(Str, [Missing]),
	f10 : Try(Str, [Missing]),
	f11 : Try(Str, [Missing]),
}

source : Str
source = "{\"f00\":\"p0\",\"f01\":\"p1\",\"f02\":\"p2\",\"f03\":\"p3\",\"f04\":\"p4\",\"f05\":\"p5\",\"f06\":\"p6\",\"f07\":\"p7\",\"f08\":\"p8\",\"f09\":\"p9\",\"f10\":\"p10\",\"f11\":\"p11\"}"

round_trips : Str -> Bool
round_trips = |json| {
	first_result : Try(Shape, [InvalidJson(Str)])
	first_result = Json.parse(json)

	match first_result {
		Ok(first) => {
			encoded1 = Json.to_str(first)
			second_result : Try(Shape, [InvalidJson(Str)])
			second_result = Json.parse(encoded1)

			match second_result {
				Ok(second) => Str.is_eq(Json.to_str(second), encoded1)
				Err(_) => False
			}
		}
		Err(_) => False
	}
}

expect round_trips(source)
