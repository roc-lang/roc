import FxOsStr exposing [FxOsStr]

FxEnv := [].{
    get : Str -> Try(FxOsStr, [VarNotFound(FxOsStr), EnvErr])
    get = |input|
        if Str.is_empty(input) {
            Err(VarNotFound(Utf8(input)))
        } else if Str.starts_with(input, "!") {
            Err(EnvErr)
        } else {
            Ok(WindowsU16s([]))
        }

    var_str : Str -> Try(Str, [VarNotFound(FxOsStr), EnvErr, InvalidStr(U64)])
    var_str = |input| {
        match get(input) {
            Ok(value) =>
                match FxOsStr.to_str_try(value) {
                    Ok(str) => Ok(str)
                    Err(InvalidStr(index)) => Err(InvalidStr(index))
                }
            Err(VarNotFound(name)) => Err(VarNotFound(name))
            Err(EnvErr) => Err(EnvErr)
        }
    }
}
