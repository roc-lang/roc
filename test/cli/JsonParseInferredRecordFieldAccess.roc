JsonParseInferredRecordFieldAccess :: [].{}

# Each decoded record is only ever used through field access, so its row is
# never closed by the program; nothing in this file has a type annotation.
greet = |json| {
	person = Json.parse(json)?
	Ok(Str.concat("Hello, ", person.name))
}

expect greet("{\"name\":\"Ann\"}") == Ok("Hello, Ann")

nested = |json| {
	rec = Json.parse(json)?
	Ok(Str.concat(rec.person.name, "!"))
}

expect nested("{\"person\":{\"name\":\"Ann\"}}") == Ok("Ann!")

names = |json| {
	rec = Json.parse(json)?
	Ok(List.map(rec.people, |p| Str.concat(p.name, "!")))
}

expect names("{\"people\":[{\"name\":\"A\"},{\"name\":\"B\"}]}") == Ok(["A!", "B!"])

city = |json| {
	{ address } = Json.parse(json)?
	Ok(Str.concat(address.city, "?"))
}

expect city("{\"address\":{\"city\":\"X\"}}") == Ok("X?")

expect {
	match city("{\"address\":{}}") {
		Err(MissingRequiredField(field)) => field == "city"
		_ => False
	}
}

reencode = |json| {
	rec = Json.parse(json)?
	_ = Str.concat("", rec.person.name)
	Ok(Json.to_str(rec))
}

expect reencode("{\"person\":{\"name\":\"A\"}}") == Ok("{\"person\":{\"name\":\"A\"}}")

expect {
	match Json.parse("{\"person\":{\"name\":\"Ann\"}}") {
		Ok(rec) => rec.person.name == "Ann"
		Err(_) => False
	}
}
