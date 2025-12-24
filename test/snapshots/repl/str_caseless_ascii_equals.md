# META
~~~ini
description=Str.caseless_ascii_equals should work with various string combinations
type=repl
~~~
# SOURCE
~~~roc
» Str.caseless_ascii_equals("Hello", "hello")
» Str.caseless_ascii_equals("Hello", "HELLO")
» Str.caseless_ascii_equals("Éaaa", "ÉAAA")
» Str.caseless_ascii_equals("Hello there", "World")
» Str.caseless_ascii_equals("ÉÉÉ", "ééé")
~~~
# OUTPUT
True
---
True
---
True
---
False
---
False
# PROBLEMS
NIL
