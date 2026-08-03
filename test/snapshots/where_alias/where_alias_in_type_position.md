# META
~~~ini
description=Using a where alias as a type is rejected
type=snippet
~~~
# SOURCE
~~~roc
a.Stringable : where [a.to_str : a -> Str]

describe : Stringable -> Str
describe = |value| value.to_str()
~~~
