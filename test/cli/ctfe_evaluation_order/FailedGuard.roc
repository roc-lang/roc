import Guarded

FailedGuard := [].{
    result : I64
    result = Guarded.choose(Bool.True)
}

expect FailedGuard.result == 7.I64
