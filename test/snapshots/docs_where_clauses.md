# META
~~~ini
description=Functions with where clause constraints
type=docs
~~~
# SOURCE
## app.roc
~~~roc
app [stringify, compare, main] { pf: platform "./platform.roc" }

## Convert any value to a string.
stringify : a -> Str where [a.to_str : a -> Str]
stringify = |value| value.to_str()

## Check equality of two values.
compare : a, a -> Bool where [a.is_eq : a, a -> Bool]
compare = |x, y| x.is_eq(y)

main = "test"
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
      (name "stringify")
      (kind value)
      (type "a -> Str where [a.to_str : a -> Str]")
      (doc "Convert any value to a string.")
    )
    (entry
      (name "compare")
      (kind value)
      (type "a, a -> Bool where [a.is_eq : a, a -> Bool]")
      (doc "Check equality of two values.")
    )
    (entry
      (name "main")
      (kind value)
      (type "Str")
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
