# META
~~~ini
description=Record pattern destructuring in function parameter
type=stmt
~~~
# SOURCE
~~~roc
formatUser = |{ name, age, email }| "User: $(name) ($(age.toStr()) years old) - Contact: $(email.display())"
~~~
