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
[Some(42), None, Ok("hello"), Err("oops"), Just(100), Nothing, Left(1), Right(2), Some(Ok(Just(42))), Result(Ok(Some(True)))]
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - tag_applications_simple.md:3:5:3:5
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.list_literal)
~~~
# SOLVED
~~~clojure
; Total type variables: 36
(var #0 _)
(var #1 _)
(var #2 Num *)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 Str)
(var #7 _)
(var #8 _)
(var #9 Str)
(var #10 _)
(var #11 _)
(var #12 Num *)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 Num *)
(var #17 _)
(var #18 _)
(var #19 Num *)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 Num *)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
~~~
# TYPES
~~~roc
~~~
