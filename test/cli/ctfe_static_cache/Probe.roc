import Static

Probe := [].{
    value : I64
    value = Static.read(0) + Static.read(1)

    guarded_value : I64
    guarded_value = Static.guarded(1) + Static.guarded(2)

    validated : {}
    validated = {
        expect Probe.value == 233.I64
        expect Probe.guarded_value == 705.I64
        {}
    }
}

expect Probe.value == 233.I64

expect Probe.guarded_value == 705.I64

validation = Probe.validated
