# META
~~~ini
description=Type module visibility: only the main type and its children are documented
type=docs
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

import Color

main = Color.to_str(Color.red())
~~~
## Color.roc
~~~roc
## A color value.
Color := [Red, Green, Blue].{
    ## The red color.
    red : {} -> Color
    red = |{}| Red

    ## Convert a color to a string.
    to_str : Color -> Str
    to_str = |color|
        match color {
            Red => color_name(Red)
            Green => color_name(Green)
            Blue => color_name(Blue)
        }
}

## This helper is private to the module and should NOT appear in docs.
color_name : [Red, Green, Blue] -> Str
color_name = |tag|
    match tag {
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
    (package "app")
    (kind app)
    (entry
      (name "main")
      (kind value)
      (type (type-ref (name "Str")))
    )
  )
  (module
    (name "Color")
    (package "app")
    (kind type_module)
    (doc "A color value.")
    (entry
      (name "Color")
      (kind nominal)
      (type "Color := " (tag-union (tag "Red") (tag "Green") (tag "Blue")))
      (doc "A color value.")
      (entry
        (name "red")
        (kind value)
        (type (fn (record) (type-ref (module "app.Color") (name "Color"))))
        (doc "The red color.")
      )
      (entry
        (name "to_str")
        (kind value)
        (type (fn (type-ref (module "app.Color") (name "Color")) (type-ref (name "Str"))))
        (doc "Convert a color to a string.")
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
