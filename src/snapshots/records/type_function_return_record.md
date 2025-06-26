# META
~~~ini
description=Function type annotation returning record
type=stmt
~~~
# SOURCE
~~~roc
createUser! : Str, U32 => { name : Str, age : U32, id : U64, active : Bool }
~~~
