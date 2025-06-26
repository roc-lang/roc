# META
~~~ini
description=Record with special character fields (error cases)
type=expr
~~~
# SOURCE
~~~roc
{ _privateField: "leading underscore", field_: "trailing underscore", PascalCase: "pascal", kebab-case: "kebab", field$special: "dollar", field@symbol: "at symbol" }
~~~
