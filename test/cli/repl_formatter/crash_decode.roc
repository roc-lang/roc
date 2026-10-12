module [decode, encode]

decode : Str -> [Evaluate(Str), Reply(Str)]
decode = |line| crash "decode failed"

encode = |_, _| "unexpected encode"
