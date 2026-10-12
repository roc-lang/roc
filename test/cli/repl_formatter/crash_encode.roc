module [decode, encode]

decode : Str -> [Evaluate(Str), Reply(Str)]
decode = |line| Evaluate(line)

encode = |_, _| crash "encode failed"
