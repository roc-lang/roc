# META
~~~ini
description=Numeric sum then convert to I16 string
type=repl
~~~

# NOTES
This test demonstrates numeric operations in the REPL where the final usage
constrains the type to I16.

The numeric literals `a`, `b`, and `sum` are polymorphic until `I16.to_str(sum)`
constrains the result to I16.

# SOURCE
~~~roc
» a = 4
» b = 5
» sum = a + b
» I16.to_str(sum)
~~~
# OUTPUT
assigned `a`
---
assigned `b`
---
assigned `sum`
---
"9"
# PROBLEMS
NIL
