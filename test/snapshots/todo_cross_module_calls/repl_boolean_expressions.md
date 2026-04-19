# META
~~~ini
description=Boolean expressions and operations
type=repl
~~~
# SOURCE
~~~roc
» Bool.true # incorrect, tags must be UPPERCASE
» Bool.false
» Bool.True
» Bool.False
» !Bool.True
» !Bool.False
» Bool.True and Bool.False
» !Bool.True or !Bool.True
~~~
# OUTPUT
**DOES NOT EXIST**
`Bool.true` does not exist.

`Bool` is in scope, but it has no associated `true`.

It's referenced here:
**repl:1:1:1:10:**
```roc
Bool.true
```
^^^^^^^^^
---
**DOES NOT EXIST**
`Bool.false` does not exist.

`Bool` is in scope, but it has no associated `false`.

It's referenced here:
**repl:1:1:1:11:**
```roc
Bool.false
```
^^^^^^^^^^
---
True
---
False
---
False
---
True
---
False
---
False
# PROBLEMS
NIL
