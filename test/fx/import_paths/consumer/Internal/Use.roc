import dep.Parser.ParseErr as PE exposing [message]

Use := [].{
    identity : PE -> PE
    identity = |value| value

    text = |_| message({})
}
