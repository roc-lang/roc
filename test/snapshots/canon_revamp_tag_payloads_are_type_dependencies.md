# META
~~~ini
description=Tag payload type names count as type alias dependencies
type=snippet
skip=true
~~~
# SOURCE
~~~roc
A : [Tag(B)]
B : A
~~~

