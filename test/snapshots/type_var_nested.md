# META
~~~ini
description=Type variables nested within complex type constructors
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "platform.roc" }

# Map over Result type
map_result : Result(a, e), (a -> b) -> Result(b, e)
map_result = |result, transform| {
    match result {
        Ok(value) => Ok(transform(value))
        Err(error) => Err(error)
    }
}

# Simple identity function with type variable
identity : a -> a
identity = |x| x

# Nested type variables in records
make_pair : a, b -> { first: a, second: b }
make_pair = |x, y| { first: x, second: y }

# Function that works with lists of any type
list_length : List(_a) -> U64
list_length = |_lst| 42

# Nested Result types
wrap_in_result : a -> Result(Result(a, Str), Str)
wrap_in_result = |value| Ok(Ok(value))

main = |_| "done"
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound LowerIdent OpenRound LowerIdent CloseRound CloseRound UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly CloseCurly LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar Int LowerIdent OpColon LowerIdent OpArrow UpperIdent OpenRound UpperIdent OpenRound LowerIdent Comma UpperIdent CloseRound Comma UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound LowerIdent OpAssign OpBar Underscore OpBar String ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "map_result")
    (binop_thin_arrow
      (apply_uc
        (uc "Result")
        (tuple_literal
          (lc "a")
          (lc "e")
        )
      )
      (binop_thin_arrow
        (binop_thin_arrow
          (lc "a")
          (lc "b")
        )
        (apply_uc
          (uc "Result")
          (tuple_literal
            (lc "b")
            (lc "e")
          )
        )
      )
    )
  )
  (binop_equals
    (lc "map_result")
    (lambda
      (body
        (block
          (match <36 branches>)
          (binop_colon
            (lc "identity")
            (binop_thin_arrow
              (lc "a")
              (lc "a")
            )
          )
          (binop_equals
            (lc "identity")
            (lambda
              (body
                (lc "x")
              )
              (args
                (lc "x")
              )
            )
          )
          (binop_colon
            (lc "make_pair")
            (binop_thin_arrow
              (lc "a")
              (binop_thin_arrow
                (lc "b")
                (block
                  (binop_colon
                    (lc "first")
                    (binop_colon
                      (tuple_literal
                        (lc "a")
                        (lc "second")
                      )
                      (lc "b")
                    )
                  )
                )
              )
            )
          )
          (binop_equals
            (lc "make_pair")
            (lambda
              (body
                (block
                  (binop_colon
                    (lc "first")
                    (binop_colon
                      (tuple_literal
                        (lc "x")
                        (lc "second")
                      )
                      (lc "y")
                    )
                  )
                )
              )
              (args
                (tuple_literal
                  (lc "x")
                  (lc "y")
                )
              )
            )
          )
          (binop_colon
            (lc "list_length")
            (binop_thin_arrow
              (apply_uc
                (uc "List")
                (lc "_a")
              )
              (uc "U64")
            )
          )
          (binop_equals
            (lc "list_length")
            (lambda
              (body
                (num_literal_i32 42)
              )
              (args
                (lc "_lst")
              )
            )
          )
          (binop_colon
            (lc "wrap_in_result")
            (binop_thin_arrow
              (lc "a")
              (apply_uc
                (uc "Result")
                (tuple_literal
                  (apply_uc
                    (uc "Result")
                    (tuple_literal
                      (lc "a")
                      (uc "Str")
                    )
                  )
                  (uc "Str")
                )
              )
            )
          )
          (binop_equals
            (lc "wrap_in_result")
            (lambda
              (body
                (apply_uc
                  (uc "Ok")
                  (apply_uc
                    (uc "Ok")
                    (lc "value")
                  )
                )
              )
              (args
                (lc "value")
              )
            )
          )
          (binop_equals
            (lc "main")
            (lambda
              (body
                (str_literal_small "done")
              )
              (args
                (underscore)
              )
            )
          )
        )
      )
      (args
        (tuple_literal
          (lc "result")
          (lc "transform")
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 6:5 to 6:18

**Parse Error**
at 7:19 to 7:19

**Parse Error**
at 8:20 to 8:20

**Parse Error**
at 6:5 to 10:1

**Parse Error**
at 10:1 to 10:1

**Parse Error**
at 5:34 to 28:18

**Unsupported Node**
at 4:14 to 5:1

**Unsupported Node**
at 5:14 to 5:34

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "map_result")
    (Expr.malformed)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
