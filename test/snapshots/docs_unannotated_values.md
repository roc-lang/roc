# META
~~~ini
description=Values without type annotations show inferred types
type=docs
~~~
# SOURCE
## app.roc
~~~roc
app [x, greeting, main] { pf: platform "./platform.roc" }

## A number.
x = 42

## A greeting.
greeting = "hello"

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
      (name "x")
      (kind value)
      (type (type-ref (module "Builtin") (name "Dec")))
      (doc "A number.")
    )
    (entry
      (name "greeting")
      (kind value)
      (type (type-ref (module "Builtin") (name "Str")))
      (doc "A greeting.")
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
