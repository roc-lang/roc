# META
~~~ini
description=string refcount behavior in record field access
type=repl
~~~
# SOURCE
~~~roc
» "short"
» "This is a very long string that definitely exceeds the small string optimization limit and will require heap allocation"
» { foo: "short" }.foo
» { foo: "This is a very long string that definitely exceeds the small string optimization limit and will require heap allocation" }.foo
~~~
# OUTPUT
"short"
---
"This is a very long string that definitely exceeds the small string optimization limit and will require heap allocation"
---
"short"
---
"This is a very long string that definitely exceeds the small string optimization limit and will require heap allocation"
# PROBLEMS
NIL
