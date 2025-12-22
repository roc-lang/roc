# META
~~~ini
description=List of tags
type=repl
~~~
# SOURCE
~~~roc
» [SomeTag("with String"), OtherTag(2), MyTag]
» List.sublist([Foo, Bar, Baz, Quux], {start: 1, len: 2})
~~~
# OUTPUT
[SomeTag("with String"), OtherTag(2), MyTag]
---
[Bar, Baz]
# PROBLEMS
NIL
