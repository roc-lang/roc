Reply := [].{
    busy : Try(Str, _)
    busy = Ok("busy")

    failure = |_cause| Reply.busy

    either : [Yes(Str), No(_)]
    either = Yes("either")

    choose = |_cause| Reply.either
}

expect Reply.failure(Oops) == Ok("busy")
expect Reply.choose(Oops) == Yes("either")
