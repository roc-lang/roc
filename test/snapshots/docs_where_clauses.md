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
    (package "app")
    (kind app)
    (entry
      (name "stringify")
      (kind value)
      (type (where (fn (var "a") (type-ref (name "Str"))) (constraint "a" "to_str" (fn (var "a") (type-ref (name "Str"))))))
      (doc "Convert any value to a string.")
    )
    (entry
      (name "compare")
      (kind value)
      (type (where (fn (var "a") (var "a") (type-ref (name "Bool"))) (constraint "a" "is_eq" (fn (var "a") (var "a") (type-ref (name "Bool"))))))
      (doc "Check equality of two values.")
    )
    (entry
      (name "main")
      (kind value)
      (type (type-ref (name "Str")))
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
