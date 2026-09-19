# META
~~~ini
description=Issue 11306: package docs hide a private type with associated items declared alongside the main type
type=docs
~~~
# SOURCE
## main.roc
~~~roc
package [Blub] {}
~~~
## Blub.roc
~~~roc
Blub := [].{
	foo = 5
}

Help := [].{
	bar = 6
}
~~~
# DOCS
~~~clojure
(package-docs
  (name "test-app")
  (mod
    (name "Blub")
    (package "mod")
    (kind type_mod)
    (entry
      (name "Blub")
      (kind nominal)
      (type "Blub := " (tag-union))
      (entry
        (name "foo")
        (kind value)
        (type (type-ref (name "Dec")))
      )
    )
  )
)
~~~
