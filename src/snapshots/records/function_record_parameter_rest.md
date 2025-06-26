# META
~~~ini
description=Function with record parameter and rest pattern
type=expr
~~~
# SOURCE
~~~roc
|{ name, age, ..rest }| { greeting: "Hello $(name)", fullRecord: person, isAdult: age >= 18 }
~~~
