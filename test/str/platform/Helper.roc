# Helper module - imports Core and Utils (diamond dependency pattern)
# Diamond: Helper→Core→Utils and Helper→Utils
import Core
import Utils

Helper := [].{
    # This function calls Core.wrap internally (transitive call)
    wrap_fancy : Str -> Str
    wrap_fancy = |s| Core.wrap("fancy: ${s}")

    # This function calls Core.prefix internally (transitive call)
    prefix_fancy : Str -> Str
    prefix_fancy = |s| Core.prefix("fancy: ${s}")

    # Uses both Core and Utils - exercises diamond dependency
    # Helper→Core→Utils AND Helper→Utils
    wrap_quoted : Str -> Str
    wrap_quoted = |s| Core.wrap(Utils.quote(s))
}
