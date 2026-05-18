# META
~~~ini
description=Value definition with type annotation and doc comment
type=docs
~~~
# SOURCE
## app.roc
~~~roc
app [greet] { pf: platform "./platform.roc" }

## Greets someone by name.
greet : Str -> Str
greet = |name| "Hello, $(name)!"
~~~
## platform.roc
~~~roc
platform ""
    requires {} { greet : Str -> Str }
    exposes []
    packages {}
    provides { greet_for_host: "greet" }
    targets: {
        files: "targets/",
        exe: {
            x64glibc: [app],
        }
    }

greet_for_host : Str -> Str
greet_for_host = greet
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
      (name "greet")
      (kind value)
      (type (fn (type-ref (name "Str")) (type-ref (name "Str"))))
      (doc "Greets someone by name.")
    )
  )
  (module
    (name "platform")
    (package "pf")
    (kind platform)
    (entry
      (name "greet_for_host")
      (kind value)
      (type (fn (type-ref (name "Str")) (type-ref (name "Str"))))
    )
  )
)
~~~
