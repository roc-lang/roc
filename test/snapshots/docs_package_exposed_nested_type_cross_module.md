# META
~~~ini
description=Package docs route references between nested public types in different source mods
type=docs
~~~
# SOURCE
## main.roc
~~~roc
package [First.Foo, Second.Bar] {}
~~~
## First.roc
~~~roc
import Second

First :: [].{
    Foo :: [].{
        to_bar : Foo -> Second.Bar
        to_bar = |_| crash "not implemented"

        default_bar = Second.Bar.{ value: 0 }
    }
}
~~~
## Second.roc
~~~roc
Second :: [].{
    Bar := { value : U64 }
}
~~~
# DOCS
~~~clojure
(package-docs
  (name "test-app")
  (mod
    (name "Bar")
    (package "mod")
    (kind type_mod)
    (entry
      (name "Bar")
      (kind nominal)
      (type "Bar := " (record (field "value" (type-ref (name "U64")))))
    )
  )
  (mod
    (name "Foo")
    (package "mod")
    (kind type_mod)
    (entry
      (name "Foo")
      (kind opaque)
      (type "Foo :: " (tag-union))
      (entry
        (name "to_bar")
        (kind value)
        (type (fn (type-ref (mod "mod.Foo") (name "Foo")) (type-ref (mod "mod.Bar") (name "Bar"))))
      )
      (entry
        (name "default_bar")
        (kind value)
        (type (type-ref (mod "mod.Bar") (name "Bar")))
      )
    )
  )
)
~~~
