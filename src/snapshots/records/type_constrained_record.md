# META
~~~ini
description=Constrained record type annotation
type=stmt
~~~
# SOURCE
~~~roc
processUser! : { name : Str, age : U32, ..a } => Str
~~~
