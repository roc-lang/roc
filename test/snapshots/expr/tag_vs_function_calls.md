# META
~~~ini
description=Tag applications vs function calls
type=expr
~~~
# SOURCE
~~~roc
{
    someTag: Some(42),
    noneTag: None,
    okTag: Ok("hello"),
    errTag: Err("oops"),
    addOne: |x| x + 1,
    result: addOne(5),
    nested: Some(Ok(Just(42))),
    tagList: [Some(1), Some(2), None, Some(3)],
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon UpperIdent OpenRound Int CloseRound Comma LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent OpenRound String CloseRound Comma LowerIdent OpColon UpperIdent OpenRound String CloseRound Comma LowerIdent OpColon OpBar LowerIdent OpBar LowerIdent OpPlus Int Comma LowerIdent OpColon LowerIdent OpenRound Int CloseRound Comma LowerIdent OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent OpenRound Int CloseRound CloseRound CloseRound Comma LowerIdent OpColon OpenSquare UpperIdent OpenRound Int CloseRound Comma UpperIdent OpenRound Int CloseRound Comma UpperIdent Comma UpperIdent OpenRound Int CloseRound CloseSquare Comma CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (binop_colon
    (lc "someTag")
    (apply_uc
      (uc "Some")
      (num_literal_i32 42)
    )
  )
  (binop_colon
    (lc "noneTag")
    (uc "None")
  )
  (binop_colon
    (lc "okTag")
    (apply_uc
      (uc "Ok")
      (str_literal_big "hello")
    )
  )
  (binop_colon
    (lc "errTag")
    (apply_uc
      (uc "Err")
      (str_literal_small "oops")
    )
  )
  (binop_colon
    (lc "addOne")
    (lambda
      (body
        (tuple_literal
          (binop_colon
            (tuple_literal
              (binop_colon
                (tuple_literal
                  (binop_colon
                    (tuple_literal
                      (binop_plus
                        (lc "x")
                        (num_literal_i32 1)
                      )
                      (lc "result")
                    )
                    (apply_lc
                      (lc "addOne")
                      (num_literal_i32 5)
                    )
                  )
                  (lc "nested")
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
              )
              (lc "tagList")
            )
            (list_literal
              (apply_uc
                (uc "Some")
                (num_literal_i32 1)
              )
              (apply_uc
                (uc "Some")
                (num_literal_i32 2)
              )
              (uc "None")
              (apply_uc
                (uc "Some")
                (num_literal_i32 3)
              )
            )
          )
          (malformed malformed:expr_unexpected_token)
        )
      )
      (args
        (lc "x")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
{
	someTag : Some 42,
	noneTag : None,
	okTag : Ok "hello",
	errTag : Err "oops",
	addOne : \
		x,
	 -> ((
		(
			(
				x + 1,
				result,
			) : addOne(5),
			nested,
		) : Some Ok Just 42,
		tagList,
	) : [Some(1), Some(2), None, Some(3)]),
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 10:1 to 10:1

**Parse Error**
at 1:1 to 10:2

# CANONICALIZE
~~~clojure
(Expr.binop_double_slash)
~~~
# SOLVED
~~~clojure
(expr :tag binop_double_slash :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
