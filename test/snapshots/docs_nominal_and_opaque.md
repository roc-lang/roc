# META
~~~ini
description=Nominal and opaque type definitions with doc comments
type=docs
~~~
# SOURCE
## app.roc
~~~roc
app [Color, Id, main] { pf: platform "./platform.roc" }

## A color value.
Color := [Red, Green, Blue]

## An opaque identifier.
Id :: U64

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
      (name "main")
      (kind value)
    )
    (entry
      (name "Color")
      (kind nominal)
      (type "Color := [Red, Green, Blue]")
      (doc "A color value.")
    )
    (entry
      (name "Id")
      (kind opaque)
      (type "Id :: U64")
      (doc "An opaque identifier.")
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
