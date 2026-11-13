# META
~~~ini
description=Package docs document a private mod's nominal type under the public alias that renames it
type=docs
~~~
# SOURCE
## main.roc
~~~roc
package [Blub, Other] {}
~~~
## Blub.roc
~~~roc
import Hidden

Blub := [].{
	Token : Hidden

	make : U64 -> Token
	make = |id| Hidden.new(id)
}
~~~
## Other.roc
~~~roc
import Hidden

Other := [].{
	token_id : Hidden -> U64
	token_id = |token| token.id()
}
~~~
## Hidden.roc
~~~roc
Hidden := { id : U64 }.{
	new : U64 -> Hidden
	new = |value| Hidden.{ id: value }

	id : Hidden -> U64
	id = |token| token.id
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
        (name "make")
        (kind value)
        (type (fn (type-ref (name "U64")) (type-ref (mod "mod.Blub") (name "Blub.Token"))))
      )
      (entry
        (name "Token")
        (kind nominal)
        (type "Token := " (record (field "id" (type-ref (name "U64")))))
        (entry
          (name "new")
          (kind value)
          (type (fn (type-ref (name "U64")) (type-ref (mod "mod.Blub") (name "Blub.Token"))))
        )
        (entry
          (name "id")
          (kind value)
          (type (fn (type-ref (mod "mod.Blub") (name "Blub.Token")) (type-ref (name "U64"))))
        )
      )
    )
  )
  (mod
    (name "Other")
    (package "mod")
    (kind type_mod)
    (entry
      (name "Other")
      (kind nominal)
      (type "Other := " (tag-union))
      (entry
        (name "token_id")
        (kind value)
        (type (fn (type-ref (mod "mod.Blub") (name "Blub.Token")) (type-ref (name "U64"))))
      )
    )
  )
)
~~~
