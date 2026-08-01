# META
~~~ini
description=Derived JSON codec for a primitive-backed nominal encodes/parses as its backing scalar, and the value survives
type=repl
~~~
# SOURCE
~~~roc
» Username := Str.{ encoder_for : _ }
» name = Username.("a heap allocated username value")
» Json.to_str(name)
» name
» Age := I64.{ encoder_for : _ }
» Json.to_str(Age.(42))
» Flag := Bool.{ encoder_for : _ }
» Json.to_str(Flag.(Bool.True))
» Price := Dec.{ encoder_for : _ }
» Json.to_str(Price.(19.95))
» Token := Str.{ parser_for : _ }
» match Json.parse("\"parsed token\"") { Ok(t) => t, Err(_) => Token.("parse failed") }
~~~
# OUTPUT
assigned `Username`
---
assigned `name`
---
"\"a heap allocated username value\""
---
"a heap allocated username value"
---
assigned `Age`
---
"42"
---
assigned `Flag`
---
"true"
---
assigned `Price`
---
"19.95"
---
assigned `Token`
---
"parsed token"
# PROBLEMS
NIL
