# A nominal with a derived encoder_for, beside another field of a record
# encoded by Json.to_str_try, whose error row the use leaves open. The
# nominal's derivation nests in the record's, and both write a string field
# with the format's encode_str at that one open error row.

Badge := { name : Str }.{
	encoder_for : _
}

badge : Badge
badge = { name: "Ada" }

expect Json.to_str_try({ id: "a1", badge }) == Ok("{\"badge\":{\"name\":\"Ada\"},\"id\":\"a1\"}")
