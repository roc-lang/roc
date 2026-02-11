# META
~~~ini
description=Multiple exported values with different type signatures
type=docs
~~~
# SOURCE
## app.roc
~~~roc
app [main, add, identity] { pf: platform "./platform.roc" }

## Adds two numbers.
add : U64, U64 -> U64
add = |a, b| a + b

## Returns the input unchanged.
identity : a -> a
identity = |x| x

## The main entry point.
main : Str
main = "hello"
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
      (name "add")
      (kind value)
      (type "U64, U64 -> U64")
      (doc "Adds two numbers.")
    )
    (entry
      (name "identity")
      (kind value)
      (type "a -> a")
      (doc "Returns the input unchanged.")
    )
    (entry
      (name "main")
      (kind value)
      (type "Str")
      (doc "The main entry point.")
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
