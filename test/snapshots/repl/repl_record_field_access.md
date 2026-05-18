# META
~~~ini
description=Record field access
type=repl
~~~
# SOURCE
~~~roc
» {}.foo
» {foo: "Hello"}.foo
» {foo: "Hello", bar: "World"}.bar
~~~
# OUTPUT
**TYPE MISMATCH**
This record does not have a `foo` field:
**repl:1:1:1:3:**
```roc
{}.foo
```
^^

It is actually a record with no fields.
---
"Hello"
---
"World"
# PROBLEMS
NIL
