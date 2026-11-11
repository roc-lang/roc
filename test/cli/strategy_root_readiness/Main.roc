import Producer

Main := [].{}

consume : a, ({} -> Str) -> Str
consume = |_unrelated, callback| callback({})

expect consume("consumer scope", Producer.values.first) == "[producer scope]"
expect consume(42.U64, Producer.values.second) == "[producer scope]"
