FallibleHost := [].{
    str_ok! : {} => Try(Str, [HostErr(Str)])
    json_input! : {} => Str
}
