# META
~~~ini
description=Function with record parameter destructuring and rest pattern, capture whole record using as
type=expr
~~~
# SOURCE
~~~roc
|{ name, age, .. } as person| { greeting: "Hello $(name)", fullRecord: person, isAdult: age >= 18 }
~~~
