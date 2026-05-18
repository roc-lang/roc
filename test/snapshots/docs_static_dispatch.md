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
    (package "app")
    (kind app)
    (entry
      (name "main")
      (kind value)
      (type (error))
    )
  )
  (module
    (name "Counter")
    (package "app")
    (kind type_module)
    (entry
      (name "Counter")
      (kind nominal)
      (type "Counter := " (record (field "count" (type-ref (name "U64")))))
      (entry
        (name "new")
        (kind value)
        (type (fn (record) (type-ref (module "app.Counter") (name "Counter"))))
      )
      (entry
        (name "increment")
        (kind value)
        (type (fn (type-ref (module "app.Counter") (name "Counter")) (type-ref (module "app.Counter") (name "Counter"))))
      )
      (entry
        (name "value")
        (kind value)
        (type (fn (type-ref (module "app.Counter") (name "Counter")) (type-ref (name "U64"))))
      )
    )
  )
  (module
    (name "platform")
    (package "pf")
    (kind platform)
    (entry
      (name "main_for_host")
      (kind value)
      (type (type-ref (name "Str")))
    )
  )
)
~~~
