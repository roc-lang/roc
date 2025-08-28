# META
~~~ini
description=Type variables and values exist in separate namespaces
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

# Type variable 'elem' introduced in annotation
process : List(elem) -> elem
process = |list| {
    # value identifier named 'elem' is allowed - different namespace from type variable
    elem = 42

    # type variable 'elem' still refers to the function annotation's type parameter
    result : elem
    result = List.first(list) |> Result.withDefault(elem)

    result
}

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign Int LowerIdent OpColon LowerIdent LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound OpBar OpGreaterThan UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound LowerIdent CloseCurly LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "process")
    (binop_thin_arrow
      (apply_uc
        (uc "List")
        (lc "elem")
      )
      (lc "elem")
    )
  )
  (binop_equals
    (lc "process")
    (lambda
      (body
        (block
          (binop_equals
            (lc "elem")
            (num_literal_i32 42)
          )
          (binop_colon
            (lc "result")
            (lc "elem")
          )
          (apply_anon
            (binop_pipe
              (uc "List")
              (dot_lc "first")
            )
            (lc "list")
          )
          (apply_anon
            (binop_pipe
              (malformed malformed:expr_unexpected_token)
              (dot_lc "withDefault")
            )
            (lc "elem")
          )
          (lc "result")
        )
      )
      (args
        (lc "list")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (record_literal)
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
	pf: "../basic-cli/platform.roc" platform [main],
}

process : List elem -> elem
process = \list -> {
		# value identifier named 'elem' is allowed - different namespace from type variable
elem = 42
	result : elem
	List.first(list)
	 | .withDefault(elem)
	result
}
main! = \_ -> {  }
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 11:32 to 11:32

**Parse Error**
at 11:34 to 11:34

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
