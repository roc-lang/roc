# META
~~~ini
description="The test case from the original request, involving multiple levels of nesting and local assignments. This will be the ultimate validation."
type=expr
~~~
# SOURCE
~~~roc
(((|a| {
    a_loc = a * 2
    |b| {
        b_loc = a_loc + b
        |c| b_loc + c
    }
})(100))(20))(3)
~~~
# TOKENS
~~~text
OpenRound OpenRound OpenRound OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign LowerIdent OpStar Int OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign LowerIdent OpPlus LowerIdent OpBar LowerIdent OpBar LowerIdent OpPlus LowerIdent CloseCurly CloseCurly CloseRound OpenRound Int CloseRound CloseRound OpenRound Int CloseRound CloseRound OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (apply_anon
    (apply_anon
      (lambda
        (body
          (block
            (num_literal_i32 2)
            (lambda
              (body
                (block
                  (binop_colon
                    (lc "b")
                    (lc "b")
                  )
                  (lambda
                    (body
                      (binop_plus
                        (lc "b_loc")
                        (lc "c")
                      )
                    )
                    (args
                      (lc "c")
                    )
                  )
                )
              )
              (args
                (lc "b")
              )
            )
          )
        )
        (args
          (lc "a")
        )
      )
      (num_literal_i32 100)
    )
    (num_literal_i32 20)
  )
  (num_literal_i32 3)
)
~~~
# FORMATTED
~~~roc
(|a| {
	2
	|b| {
		b : b
		|c| b_loc + c
	}
})(100)(20)(3)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.apply_ident)
~~~
# SOLVED
~~~clojure
(expr :tag apply_ident :type "_d")
~~~
# TYPES
~~~roc
_d
~~~
