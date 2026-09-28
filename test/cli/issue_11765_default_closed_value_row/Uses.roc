import Reply

Uses := [].{
    reply = |cause| Reply.failure(cause)
}

expect Uses.reply(Oops) == Ok("busy")
expect Reply.failure(Oops) == Ok("busy")
expect Reply.choose(Oops) == Yes("either")
