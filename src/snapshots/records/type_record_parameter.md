# META
~~~ini
description=Function type annotation with record parameter
type=stmt
~~~
# SOURCE
~~~roc
processUserThings : { name : Str, age : U32, thing: a }, (a -> Str) -> Str
~~~
