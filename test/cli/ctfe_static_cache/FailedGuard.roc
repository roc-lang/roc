import Static

FailedGuard := [].{
    value : I64
    value = Static.guarded(0)
}

expect FailedGuard.value == 0.I64
