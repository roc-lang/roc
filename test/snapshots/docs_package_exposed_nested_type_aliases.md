# META
~~~ini
description=Package docs project nested source types through their public aliases
type=docs
~~~
# SOURCE
## main.roc
~~~roc
package [Container.Blub as Foo, Container.Other as Bar] {}
~~~
## Container.roc
~~~roc
## Private parent documentation.
Container :: [].{
    ## Public Blub documentation.
    Blub :: [].{
        to_other : Blub -> Other
        to_other = |_| crash "not implemented"
    }

    ## Public Other documentation.
    Other :: [].{
        to_blub : Other -> Blub
        to_blub = |_| crash "not implemented"
    }

    Private :: [].{}
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
    (doc "Public Other documentation.")
    (entry
      (name "Bar")
      (kind opaque)
      (type "Bar :: " (tag-union))
      (doc "Public Other documentation.")
      (entry
        (name "to_blub")
        (kind value)
        (type (fn (type-ref (mod "mod.Bar") (name "Bar")) (type-ref (mod "mod.Foo") (name "Foo"))))
      )
    )
  )
  (mod
    (name "Foo")
    (package "mod")
    (kind type_mod)
    (doc "Public Blub documentation.")
    (entry
      (name "Foo")
      (kind opaque)
      (type "Foo :: " (tag-union))
      (doc "Public Blub documentation.")
      (entry
        (name "to_other")
        (kind value)
        (type (fn (type-ref (mod "mod.Foo") (name "Foo")) (type-ref (mod "mod.Bar") (name "Bar"))))
      )
    )
  )
)
~~~
