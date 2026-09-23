# META
~~~ini
description=Type mod docs keep the main type's associated items and hide private siblings' items when either is referenced before its declaration
type=docs
~~~
# SOURCE
## main.roc
~~~roc
package [Blub] {}
~~~
## Blub.roc
~~~roc
early = Blub.baz

Early := [].{
	from_early = Blub.baz
}

Blub := [].{
	foo = Help.bar
	baz = 1
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
      (entry
        (name "baz")
        (kind value)
        (type (type-ref (name "Dec")))
      )
    )
  )
)
~~~
