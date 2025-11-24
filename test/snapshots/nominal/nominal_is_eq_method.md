# META
~~~ini
description=Nominal type with is_eq method for equality comparison
type=repl
~~~
# SOURCE
~~~roc
» MyColor := [Red, Green, Blue].{ is_eq = |a, b| match (a, b) { (Red, Red) => True (Green, Green) => True (Blue, Blue) => True _ => False } }
» red = MyColor.Red
» green = MyColor.Green
» red == red
» red == green
» red != green
» green == green
~~~
# OUTPUT
MyColor
---
assigned `red`
---
assigned `green`
---
True
---
False
---
True
---
True
# PROBLEMS
NIL
