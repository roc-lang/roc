# META
~~~ini
description=Unannotated definitions with complex inferred types
type=docs
~~~
# SOURCE
## app.roc
~~~roc
app [make_pair, get_name, numbers, main] { pf: platform "./platform.roc" }

## Makes a record from components.
make_pair = |a, b| { first: a, second: b }

## Gets the name field.
get_name = |record| record.name

## A list of numbers.
numbers = [1, 2, 3]

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
      (name "make_pair")
      (kind value)
      (type (fn (var "a") (var "b") (record (field "first" (var "a")) (field "second" (var "b")))))
      (doc "Makes a record from components.")
    )
    (entry
      (name "get_name")
      (kind value)
      (type (fn (record (ext (var "a")) (field "name" (var "b"))) (var "b")))
      (doc "Gets the name field.")
    )
    (entry
      (name "numbers")
      (kind value)
      (type (apply (type-ref (module "Builtin") (name "List")) (type-ref (module "Builtin") (name "Dec"))))
      (doc "A list of numbers.")
    )
    (entry
      (name "main")
      (kind value)
      (type (type-ref (module "Builtin") (name "Str")))
    )
  )
  (module
    (name "platform")
    (package "pf")
    (kind platform)
    (entry
      (name "main_for_host")
      (kind value)
      (type (type-ref (module "Builtin") (name "Str")))
    )
  )
)
~~~
