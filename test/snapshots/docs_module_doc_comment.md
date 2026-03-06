# META
~~~ini
description=Module-level doc comment extraction
type=docs
~~~
# SOURCE
## app.roc
~~~roc
## This module provides greeting utilities.
app [greet] { pf: platform "./platform.roc" }

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
    (doc "This module provides greeting utilities.")
    (entry
      (name "greet")
      (kind value)
      (type (fn (type-ref (name "Str")) (type-ref (name "Str"))))
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
