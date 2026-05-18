# META
~~~ini
description=Types from transitive module imports
type=docs
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

import Geometry

main = Geometry.describe(Geometry.unit())
~~~
## Geometry.roc
~~~roc
import Helpers

## A rectangle with width and height.
Geometry := { width: U64, height: U64 }.{
    ## A unit rectangle.
    unit : {} -> Geometry
    unit = |{}| { width: 1, height: 1 }

    ## Calculate the area of a rectangle.
    area : Geometry -> U64
    area = |{ width, height }| width * height

    ## Describe the area as a string.
    describe : Geometry -> Str
    describe = |geo| Helpers.show(Geometry.area(geo))
}
~~~
## Helpers.roc
~~~roc
## String display utilities.
Helpers := {}.{
    ## Show a number as a string.
    show : U64 -> Str
    show = |n| Num.to_str(n)
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
      (type (type-ref (name "Str")))
    )
  )
  (module
    (name "Geometry")
    (package "app")
    (kind type_module)
    (entry
      (name "Geometry")
      (kind nominal)
      (type "Geometry := " (record (field "width" (type-ref (name "U64"))) (field "height" (type-ref (name "U64")))))
      (doc "A rectangle with width and height.")
      (entry
        (name "unit")
        (kind value)
        (type (fn (record) (type-ref (module "app.Geometry") (name "Geometry"))))
        (doc "A unit rectangle.")
      )
      (entry
        (name "area")
        (kind value)
        (type (fn (type-ref (module "app.Geometry") (name "Geometry")) (type-ref (name "U64"))))
        (doc "Calculate the area of a rectangle.")
      )
      (entry
        (name "describe")
        (kind value)
        (type (fn (type-ref (module "app.Geometry") (name "Geometry")) (type-ref (name "Str"))))
        (doc "Describe the area as a string.")
      )
    )
  )
  (module
    (name "Helpers")
    (package "app")
    (kind type_module)
    (doc "String display utilities.")
    (entry
      (name "Helpers")
      (kind nominal)
      (type "Helpers := " (record))
      (doc "String display utilities.")
      (entry
        (name "show")
        (kind value)
        (type (fn (type-ref (name "U64")) (type-ref (name "Str"))))
        (doc "Show a number as a string.")
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
