# META
~~~ini
description=Package docs document a private nominal type under the public alias that renames it
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
	## How a blub can go wrong.
	Problem : BlubProblem

	Wrapped(item) : BlubWrapped(item)

	## An alias of a public type stays an alias.
	Same : Blub

	## An alias that picks the argument stays an alias.
	Bytes : BlubWrapped(U8)

	check : Blub -> Try({}, Problem)
	check = |_| Err(BlubProblem.TooBig)

	describe : BlubProblem -> Str
	describe = |problem| problem.message()
}

BlubProblem := [TooBig, TooSmall].{
	message : BlubProblem -> Str
	message = |problem| match problem {
		TooBig => "too big"
		TooSmall => "too small"
	}

	Detail : BlubDetail
}

BlubDetail := { line : U64 }.{
	line_of : BlubDetail -> U64
	line_of = |detail| detail.line
}

BlubWrapped(elem) := [Wrap(elem)].{
	unwrap : BlubWrapped(elem) -> elem
	unwrap = |Wrap(value)| value
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
        (name "check")
        (kind value)
        (type (fn (type-ref (mod "mod.Blub") (name "Blub")) (apply (type-ref (name "Try")) (record) (type-ref (mod "mod.Blub") (name "Blub.Problem")))))
      )
      (entry
        (name "describe")
        (kind value)
        (type (fn (type-ref (mod "mod.Blub") (name "Blub.Problem")) (type-ref (name "Str"))))
      )
      (entry
        (name "Problem")
        (kind nominal)
        (type "Problem := " (tag-union (tag "TooBig") (tag "TooSmall")))
        (doc "How a blub can go wrong.")
        (entry
          (name "message")
          (kind value)
          (type (fn (type-ref (mod "mod.Blub") (name "Blub.Problem")) (type-ref (name "Str"))))
        )
        (entry
          (name "Detail")
          (kind nominal)
          (type "Detail := " (record (field "line" (type-ref (name "U64")))))
          (entry
            (name "line_of")
            (kind value)
            (type (fn (type-ref (mod "mod.Blub") (name "Blub.Problem.Detail")) (type-ref (name "U64"))))
          )
        )
      )
      (entry
        (name "Wrapped")
        (kind nominal)
        (type "Wrapped := " (tag-union (tag "Wrap" (var "elem"))))
        (entry
          (name "unwrap")
          (kind value)
          (type (fn (apply (type-ref (mod "mod.Blub") (name "Blub.Wrapped")) (var "elem")) (var "elem")))
        )
      )
      (entry
        (name "Same")
        (kind alias)
        (type "Same : " (type-ref (mod "mod.Blub") (name "Blub")))
        (doc "An alias of a public type stays an alias.")
      )
      (entry
        (name "Bytes")
        (kind alias)
        (type "Bytes : " (apply (type-ref (mod "mod.Blub") (name "Blub.Wrapped")) (type-ref (name "U8"))))
        (doc "An alias that picks the argument stays an alias.")
      )
    )
  )
)
~~~
