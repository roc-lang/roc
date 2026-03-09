import RecordField exposing [RecordField]

RecordRepr := { alignment : U64, alt_alignment : U64, alt_fields : List(RecordField), alt_size : U64, fields : List(RecordField), name : Str, size : U64 }
