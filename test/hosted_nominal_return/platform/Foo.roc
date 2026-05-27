Foo := {
    label : Str,
}.{
    Idx := { idx : U32 }.{
        get! : Idx => Foo
    }
}
