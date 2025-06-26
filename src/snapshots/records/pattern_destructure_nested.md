# META
~~~ini
description=Nested record destructuring pattern
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name, address: { street, city, zipCode } } => "$(name) lives on $(street) in $(city)"
}
~~~
