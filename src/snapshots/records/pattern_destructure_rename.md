# META
~~~ini
description=Record destructuring with field renaming
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name: userName, age: userAge } => "User $(userName) is $(Num.toStr userAge) years old"
}
~~~
