# META
~~~ini
description=Package docs document the associated methods of a private mod's nested opaque type under the public alias that renames it (https://github.com/roc-lang/roc/issues/13018)
type=docs
~~~
# SOURCE
## main.roc
~~~roc
package [Public] {}
~~~
## Public.roc
~~~roc
import Hidden

## The app-facing surface.
Public := [].{
	## A value the app can construct and read.
	Parcel(a) : Hidden.Parcel(a)
}

expect Public.Parcel.create(42.U64).get() == 42
~~~
## Hidden.roc
~~~roc
## The private implementation mod.
Hidden := [].{
	## A wrapped value with two public associated methods.
	Parcel(a) :: { value : a }.{
		## Construct a box holding the value.
		create : a -> Parcel(a)
		create = |value| { value: value }

		## Read the value held by a box.
		get : Parcel(a) -> a
		get = |box| box.value
	}
}
~~~
# DOCS
~~~clojure
(package-docs
  (name "test-app")
  (mod
    (name "Public")
    (package "mod")
    (kind type_mod)
    (entry
      (name "Public")
      (kind nominal)
      (type "Public := " (tag-union))
      (doc "The app-facing surface.")
      (entry
        (name "Parcel")
        (kind opaque)
        (type "Parcel :: " (record (field "value" (var "a"))))
        (doc "A value the app can construct and read.")
        (entry
          (name "create")
          (kind value)
          (type (fn (var "a") (apply (type-ref (mod "mod.Public") (name "Public.Parcel")) (var "a"))))
          (doc "Construct a box holding the value.")
        )
        (entry
          (name "get")
          (kind value)
          (type (fn (apply (type-ref (mod "mod.Public") (name "Public.Parcel")) (var "a")) (var "a")))
          (doc "Read the value held by a box.")
        )
      )
    )
  )
)
~~~
