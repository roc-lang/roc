import RecordField exposing [RecordField]

RecordRepr := {
	alignment : U64,
	alignment_32 : U64,
	alignment_64 : U64,
	anonymous : Bool,
	fields : List(RecordField),
	name : Str,
	size : U64,
	size_32 : U64,
	size_64 : U64,
}
