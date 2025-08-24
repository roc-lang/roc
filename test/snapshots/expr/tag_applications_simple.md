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
  (tuple_literal
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
    (malformed malformed:expr_unexpected_token)
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - tag_applications_simple.md:3:5:3:5
# PROBLEMS
**Parse Error**
at 12:1 to 12:1

**Parse Error**
at 1:1 to 12:2

**Unsupported Node**
at 1:1 to 12:1

# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
(expr :tag malformed :type "Error")
~~~
# TYPES
~~~roc
Error
~~~
