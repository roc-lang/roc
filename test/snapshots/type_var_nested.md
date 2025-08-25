# META
~~~ini
description=Type variables nested within complex type constructors
type=file
~~~
# SOURCE
~~~roc
app { pf: "platform.roc" platform [main] }

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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent CloseSquare CloseCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound LowerIdent OpenRound LowerIdent CloseRound CloseRound UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly CloseCurly LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar Int LowerIdent OpColon LowerIdent OpArrow UpperIdent OpenRound UpperIdent OpenRound LowerIdent Comma UpperIdent CloseRound Comma UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound LowerIdent OpAssign OpBar Underscore OpBar String ~~~
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
        (binop_thin_arrow
          (record_literal
            (match <38 branches>)
            (binop_colon
              (lc "identity")
              (lc "a")
            )
            (malformed malformed:expr_unexpected_token)
            (binop_colon
              (lc "a")
              (lc "a")
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
              (lc "a")
            )
            (lc "b")
          )
          (record_literal
            (binop_colon
              (lc "first")
              (lc "a")
            )
            (binop_colon
              (lc "second")
              (lc "b")
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
  (binop_equals
    (lc "make_pair")
    (lambda
      (body
        (record_literal
          (binop_colon
            (lc "first")
            (lc "x")
          )
          (binop_colon
            (lc "second")
            (lc "y")
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
~~~
# FORMATTED
~~~roc
app
{
	pf: "platform.roc" platform [
		main,
	],
}

map_result: (Result((a, e)) -> ((a -> b) -> Result((b, e))))
map_result = \(
	result,
	transform
) -> {
	when result is {
		Ok(value)
		=>
		Ok(transform(value))
		Err(error)
		=>
		Err(error)
	} -> }

# Simple identity function with type variable,
	identity: a,
	a: a,
	identity = \x -> x,
	make_pair: a,
	b
} -> { first: a, second: b }
make_pair = \(
	x,
	y
) -> { first: x, second: y }
list_length: (List(_a) -> U64)
list_length = \_lst -> 42
wrap_in_result: (a -> Result((Result((a, Str)), Str)))
wrap_in_result = \value -> Ok(Ok(value))
main = \_ -> "done"
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
at 13:14 to 13:14

**Parse Error**
at 5:34 to 17:18

**Unsupported Node**
at 4:14 to 5:1

**Unsupported Node**
at 5:14 to 5:34

**Unsupported Node**
at 18:13 to 18:20

**Unsupported Node**
at 21:15 to 21:30

**Unsupported Node**
at 22:15 to 22:22

**Unsupported Node**
at 25:18 to 26:1

**Unsupported Node**
at 26:18 to 26:26

**Unsupported Node**
at 28:8 to 28:12

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "map_result")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "list_length")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "wrap_in_result")
    (Expr.malformed)
  )
  (Expr.malformed)
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
