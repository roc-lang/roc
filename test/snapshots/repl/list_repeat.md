# META
~~~ini
description=List.repeat - repeat an element n times
type=repl
~~~
# SOURCE
~~~roc
» List.repeat(4,7)
» List.repeat("Short string", 3)
» List.repeat("This is quite the long string", 7)
» List.repeat(("Hello", 4) ,7)
» List.repeat(0,100)
» List.repeat("",0)
» List.repeat({a: "Foo", b: { c: 11}}, 2)
» x = List.repeat("🚀",5)
» List.repeat(x,2)
~~~
# OUTPUT
[4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0]
---
["Short string", "Short string", "Short string"]
---
["This is quite the long string", "This is quite the long string", "This is quite the long string", "This is quite the long string", "This is quite the long string", "This is quite the long string", "This is quite the long string"]
---
[("Hello", 4.0), ("Hello", 4.0), ("Hello", 4.0), ("Hello", 4.0), ("Hello", 4.0), ("Hello", 4.0), ("Hello", 4.0)]
---
[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
---
[]
---
[{ a: "Foo", b: { c: 11.0 } }, { a: "Foo", b: { c: 11.0 } }]
---
assigned `x`
---
[["🚀", "🚀", "🚀", "🚀", "🚀"], ["🚀", "🚀", "🚀", "🚀", "🚀"]]
# PROBLEMS
NIL
