import RecordFieldInfo exposing [RecordFieldInfo]

HostedFunctionInfo := {
    arg_fields : List(RecordFieldInfo),
    arg_type_ids : List(U64),
    index : U64,
    name : Str,
    ret_fields : List(RecordFieldInfo),
    ret_type_id : U64,
    type_str : Str,
}
