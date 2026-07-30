import HostValue exposing [HostValue]

Capability(a) := [Capability(HostValue.CapabilityHandle)].{
    new_with_eq : (a, a -> Bool) -> Capability(a)
    new_with_eq = |is_equal| {
        clone : HostValue -> HostValue
        clone = |host_value| host_value

        eq : HostValue, HostValue -> Bool
        eq = |_, _| {
            _ = is_equal
            crash "not called"
        }

        drop : HostValue -> {}
        drop = |_| {}

        Capability({ clone: Box.box(clone), eq: Box.box(eq), drop: Box.box(drop) })
    }

    new : {} -> Capability(a)
        where [
            a.is_eq : a, a -> Bool,
        ]
    new = |_| Capability.new_with_eq(|left, right| left.is_eq(right))

    handle : Capability(a) -> HostValue.CapabilityHandle
    handle = |Capability(handle_value)| handle_value

    store : Box(a), Capability(a) -> HostValue
    store = |boxed, cap| HostValue.store_with_capability!(boxed, Capability.handle(cap))

    get : HostValue, Capability(a) -> Box(a)
    get = |host_value, cap| HostValue.get_with_capability!(host_value, Capability.handle(cap))

    take : HostValue, Capability(a) -> Box(a)
    take = |host_value, cap| HostValue.take_with_capability!(host_value, Capability.handle(cap))
}
