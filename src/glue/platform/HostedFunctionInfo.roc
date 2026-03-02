import RecordFieldInfo exposing [RecordFieldInfo]

HostedFunctionInfo := {
    arg_fields : List(RecordFieldInfo),
    index : U64,
    name : Str,
    ret_fields : List(RecordFieldInfo),
    type_str : Str,
}
