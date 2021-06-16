interface Pid.Internal
    exposes [
        Pid,
        Raw,
        fromRaw,
        toRaw,
    ]
    imports []

Raw : U32

Pid : [ @Pid Raw ]

fromRaw : Raw -> Pid
fromRaw = \raw -> @Pid raw

toRaw : Pid -> Raw
toRaw = \@Pid raw -> raw
