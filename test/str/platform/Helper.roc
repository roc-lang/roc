# Helper module - imports Core and provides convenience functions
import Core

Helper := [].{
    # This function calls Core.wrap internally (transitive call)
    wrap_fancy : Str -> Str
    wrap_fancy = |s| Core.wrap("fancy: ${s}")

    # This function calls Core.prefix internally (transitive call)
    prefix_fancy : Str -> Str
    prefix_fancy = |s| Core.prefix("fancy: ${s}")
}
