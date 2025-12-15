# Core module - provides fundamental string operations
Core := [].{
    wrap : Str -> Str
    wrap = |s| "[${s}]"

    prefix : Str -> Str
    prefix = |s| "PREFIX: ${s}"
}
