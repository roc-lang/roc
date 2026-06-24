import RecordField exposing [RecordField]

RecordRepr := { alignment : U64, anonymous : Bool, fields : List(RecordField), name : Str, size : U64 }
