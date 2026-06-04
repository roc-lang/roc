Host := {
    env_prefix : Str,
    mouse : {
        x : F32,
        y : F32,
    },
}.{
    read_env! : Host, Str => Try(Str, [NotFound, ..])
}
