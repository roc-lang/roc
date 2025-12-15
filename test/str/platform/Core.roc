# Core module - provides fundamental string operations
# This is analogous to Elem.roc in roc-dom

Core := [].{
    wrap : Str -> Str
    wrap = |s| "[${s}]"

    prefix : Str -> Str
    prefix = |s| "PREFIX: ${s}"
}
