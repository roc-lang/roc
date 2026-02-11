# META
~~~ini
description=Type module with doc comments
type=docs
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

import Color

main = Color.to_str(Color.red)
~~~
## Color.roc
~~~roc
module [Color, red, to_str]

## A color value.
Color : [Red, Green, Blue]

## The red color.
red : Color
red = Red

## Convert a color to a string.
to_str : Color -> Str
to_str = |color|
    match color {
        Red => "red"
        Green => "green"
        Blue => "blue"
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
    (kind app)
    (entry
      (name "main")
      (kind value)
    )
  )
  (module
    (name "Color")
    (kind app)
    (entry
      (name "red")
      (kind value)
      (type "Color")
      (doc "The red color.")
    )
    (entry
      (name "to_str")
      (kind value)
      (type "Color -> Str")
      (doc "Convert a color to a string.")
    )
    (entry
      (name "Color")
      (kind alias)
      (type "Color : [Red, Green, Blue]")
      (doc "A color value.")
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
