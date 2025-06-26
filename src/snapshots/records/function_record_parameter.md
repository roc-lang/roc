# META
~~~ini
description=Function with record parameter destructuring
type=expr
~~~
# SOURCE
~~~roc
|{ name, age }| "Hello $(name), you are $(Num.toStr age) years old"
~~~
