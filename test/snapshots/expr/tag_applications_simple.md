# META
~~~ini
description=Various tag applications
type=expr
~~~
# SOURCE
~~~roc
[
    Some(42),
    None,
    Ok("hello"),
    Err("oops"),
    Just(100),
    Nothing,
    Left(1),
    Right(2),
    Some(Ok(Just(42))),
    Result(Ok(Some(True))),
]
~~~
# TOKENS
~~~text
OpenSquare UpperIdent OpenRound Int CloseRound Comma UpperIdent Comma UpperIdent OpenRound String CloseRound Comma UpperIdent OpenRound String CloseRound Comma UpperIdent OpenRound Int CloseRound Comma UpperIdent Comma UpperIdent OpenRound Int CloseRound Comma UpperIdent OpenRound Int CloseRound Comma UpperIdent OpenRound UpperIdent OpenRound UpperIdent OpenRound Int CloseRound CloseRound CloseRound Comma UpperIdent OpenRound UpperIdent OpenRound UpperIdent OpenRound UpperIdent CloseRound CloseRound CloseRound Comma CloseSquare ~~~
# PARSE
~~~clojure
(list_literal
  (apply_uc
    (uc "Some")
    (num_literal_i32 42)
  )
  (uc "None")
  (apply_uc
    (uc "Ok")
    (str_literal_big "hello")
  )
  (apply_uc
    (uc "Err")
    (str_literal_small "oops")
  )
  (apply_uc
    (uc "Just")
    (num_literal_i32 100)
  )
  (uc "Nothing")
  (apply_uc
    (uc "Left")
    (num_literal_i32 1)
  )
  (apply_uc
    (uc "Right")
    (num_literal_i32 2)
  )
  (apply_uc
    (uc "Some")
    (apply_uc
      (uc "Ok")
      (apply_uc
        (uc "Just")
        (num_literal_i32 42)
      )
    )
  )
  (apply_uc
    (uc "Result")
    (apply_uc
      (uc "Ok")
      (apply_uc
        (uc "Some")
        (uc "True")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
[
	Some(42),
	None,
	Ok("hello"),
	Err("oops"),
	Just(100),
	Nothing,
	Left(1),
	Right(2),
	Some(Ok(Just(42))),
	Result(Ok(Some(True))),
]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_or)
~~~
# SOLVED
~~~clojure
(expr :tag binop_or :type "[True, False]_others")
~~~
# TYPES
~~~roc
[True, False]_others
~~~
