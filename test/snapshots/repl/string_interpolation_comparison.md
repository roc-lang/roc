# META
~~~ini
description=String interpolation and concatenation comparisons
type=repl
~~~
# SOURCE
~~~roc
» name = "World"
» "Hello, ${name}!" == "Hello, World!"
» "Hello, ${name}!" == "Hello, Earth!"
» prefix = "Got: "
» suffix = "test"
» "${prefix}${suffix}" == "Got: test"
» "${prefix}${suffix}" != "Got: different"
~~~
# OUTPUT
assigned `name`
---
True
---
False
---
assigned `prefix`
---
assigned `suffix`
---
True
---
True
# PROBLEMS
NIL
