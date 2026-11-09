Suffixed := {
    filter : (Str -> Str) ?? {
        suffix = "!"
        |s| Str.concat(s, suffix)
    },
}
