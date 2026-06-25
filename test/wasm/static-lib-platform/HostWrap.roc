HostWrap := [HostWrap(U64)].{
    wrap! : U64 => HostWrap

    unwrap : HostWrap -> U64
    unwrap = |HostWrap(value)| value
}
