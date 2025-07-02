# META
~~~ini
description=Example of a nominal tag union with a payload
type=file
~~~
# SOURCE
~~~roc
module [Maybe, some, none]

Maybe(a) := [Some(a), None]

some : a -> Maybe(a)
some = |a| Maybe.Some(a)

none : Maybe(a)
none = Maybe.None
~~~
