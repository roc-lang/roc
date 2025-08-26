# META
~~~ini
description=Type application with variables in function annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

mapList : List(a), (a -> b) -> List(b)
mapList = |list, fn| list.map(fn)

main! = |_| mapList([1,2,3,4,5])
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar LowerIdent Dot LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound OpenSquare Int Comma Int Comma Int Comma Int Comma Int CloseSquare CloseRound ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "mapList")
    (binop_thin_arrow
      (apply_uc
        (uc "List")
        (lc "a")
      )
      (binop_thin_arrow
        (binop_thin_arrow
          (lc "a")
          (lc "b")
        )
        (apply_uc
          (uc "List")
          (lc "b")
        )
      )
    )
  )
  (binop_equals
    (lc "mapList")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (lc "list")
            (dot_lc "map")
          )
          (lc "fn")
        )
      )
      (args
        (tuple_literal
          (lc "list")
          (lc "fn")
        )
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (apply_lc
          (lc "mapList")
          (list_literal
            (num_literal_i32 1)
            (num_literal_i32 2)
            (num_literal_i32 3)
            (num_literal_i32 4)
            (num_literal_i32 5)
          )
        )
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
	pf: "../basic-cli/main.roc" platform [
		main,
	],
}

mapList : List a -> (a -> b) -> List b
mapList = \(list, fn) -> list.map(fn)
main! = \_ -> mapList([1, 2, 3, 4, 5])
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
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
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
