# META
~~~ini
description=Package docs preserve a dependency's public alias for inferred nested types
type=docs
~~~
# SOURCE
## main.roc
~~~roc
package [Use] { dep: "dep_main.roc" }
~~~
## dep_main.roc
~~~roc
package [Container.Blub as Thing] {}
~~~
## Container.roc
~~~roc
Container :: [].{
    Blub := { value : U64 }
}
~~~
## Use.roc
~~~roc
import dep.Thing

Use := { value : Thing }.{
    make_thing = Thing.{ value: 0 }
}
~~~
# DOCS
~~~clojure
(package-docs
  (name "test-app")
  (mod
    (name "Use")
    (package "mod")
    (kind type_mod)
    (entry
      (name "Use")
      (kind nominal)
      (type "Use := " (record (field "value" (type-ref (mod "dep.Thing") (name "Thing")))))
      (entry
        (name "make_thing")
        (kind value)
        (type (type-ref (mod "dep.Thing") (name "Thing")))
      )
    )
  )
)
~~~
