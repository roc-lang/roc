module [decode, encode]

decode : Str -> [Evaluate(Str), Reply(Str)]
decode = |line| if line == "reply" Reply("broken\nreply") else Evaluate(line)

encode = |_, _| "broken\nreply"
