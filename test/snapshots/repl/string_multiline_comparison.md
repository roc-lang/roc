# META
~~~ini
description=Simple multiline string comparison
type=repl
~~~
# SOURCE
~~~roc
» foo =
"""first line
"""second line
» foo == "first line\nsecond line"
~~~
# OUTPUT
assigned `foo`
---
True
# PROBLEMS
NIL
