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
; Total type variables: 49
(var #0 _)
(var #1 -> #36)
(var #2 Num *)
(var #3 _)
(var #4 -> #3)
(var #5 -> #37)
(var #6 Str)
(var #7 -> #3)
(var #8 -> #38)
(var #9 Str)
(var #10 -> #3)
(var #11 -> #39)
(var #12 Num *)
(var #13 -> #3)
(var #14 -> #3)
(var #15 -> #40)
(var #16 Num *)
(var #17 -> #3)
(var #18 -> #41)
(var #19 Num *)
(var #20 -> #3)
(var #21 -> #44)
(var #22 -> #43)
(var #23 -> #42)
(var #24 Num *)
(var #25 _)
(var #26 _)
(var #27 -> #3)
(var #28 -> #47)
(var #29 -> #46)
(var #30 -> #45)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 -> #3)
(var #35 -> #48)
(var #36 fn_pure)
(var #37 fn_pure)
(var #38 fn_pure)
(var #39 fn_pure)
(var #40 fn_pure)
(var #41 fn_pure)
(var #42 fn_pure)
(var #43 fn_pure)
(var #44 fn_pure)
(var #45 fn_pure)
(var #46 fn_pure)
(var #47 fn_pure)
(var #48 List #3)
~~~
# TYPES
~~~roc
~~~
