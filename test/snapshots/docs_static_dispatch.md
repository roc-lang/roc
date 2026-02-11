# META
~~~ini
description=Nominal type with static dispatch methods
type=docs
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

import Counter

main = Num.to_str(Counter.value(Counter.increment(Counter.new())))
~~~
## Counter.roc
~~~roc
Counter := { count: U64 }.{
    new : {} -> Counter
    new = |{}| { count: 0 }

    increment : Counter -> Counter
    increment = |{ count }| { count: count + 1 }

    value : Counter -> U64
    value = |{ count }| count
}
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : Str }
    exposes []
    packages {}
    provides { main_for_host: "main" }
    targets: {
        files: "targets/",
        exe: {
            x64glibc: [app],
        }
    }

main_for_host : Str
main_for_host = main
~~~
# DOCS
~~~clojure
(package-docs
  (name "test-app")
  (module
    (name "app")
    (kind app)
    (entry
      (name "main")
      (kind value)
      (type "Error")
    )
  )
  (module
    (name "Counter")
    (kind type_module)
    (entry
      (name "Counter.new")
      (kind value)
      (type "{  } -> Counter")
    )
    (entry
      (name "Counter.increment")
      (kind value)
      (type "Counter -> Counter")
    )
    (entry
      (name "Counter.value")
      (kind value)
      (type "Counter -> U64")
    )
    (entry
      (name "Counter")
      (kind nominal)
      (type "Counter := { count : U64 }")
    )
  )
  (module
    (name "platform")
    (kind platform)
    (entry
      (name "main_for_host")
      (kind value)
      (type "Str")
    )
  )
)
~~~
