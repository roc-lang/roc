# Core module - imports Utils and provides string operations
import Utils

Core := [].{
    wrap : Str -> Str
    wrap = |s| "[${s}]"

    prefix : Str -> Str
    prefix = |s| "PREFIX: ${s}"

    # Uses Utils.tag - tests Core→Utils dependency
    wrap_tagged : Str -> Str
    wrap_tagged = |s| "[${Utils.tag(s)}]"
}
