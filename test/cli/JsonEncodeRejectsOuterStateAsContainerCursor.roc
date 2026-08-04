JsonEncodeRejectsOuterStateAsContainerCursor :: [].{}

BrokenRecord := { value : Str }.{
	encoder_for : _
	encoder_for = |encoding| |record, outer_state| {
		Json.encode_record(
			encoding,
			outer_state,
			1,
			|container_state, write_field| {
				_ = container_state
				write_field(
					outer_state,
					"value",
					|value_state| Json.encode_str(encoding, record.value, value_state),
				)
			},
		)
	}
}

encoded = Json.to_str_try(BrokenRecord.{ value: "safe" })
