# META
~~~ini
description=Tag constructor names do not count as type alias dependencies
type=snippet
skip=true
~~~
# SOURCE
~~~roc
A : [B]
B : A
~~~
