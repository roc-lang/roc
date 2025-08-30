# META
~~~ini
description=Match expression with nested patterns (tags containing records, lists with tags)
type=expr
~~~
# SOURCE
~~~roc
match data {
    Container({ items: [First(x), .. as rest] }) => x + List.len(rest)
    Container({ items: [] }) => 0
    Wrapper([Tag(value), Other(y)]) => value + y
    Simple(x) => x
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpenRound OpenCurly LowerIdent OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma DoubleDot KwAs LowerIdent CloseSquare CloseCurly CloseRound OpFatArrow LowerIdent OpPlus UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound UpperIdent OpenRound OpenCurly LowerIdent OpColon OpenSquare CloseSquare CloseCurly CloseRound OpFatArrow Int UpperIdent OpenRound OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare CloseRound OpFatArrow LowerIdent OpPlus LowerIdent UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "data")
)
  (branch1     (binop_thick_arrow
      (apply_uc
        (uc "Container")
        (block
          (binop_colon
            (lc "items")
            (list_literal
              (apply_uc
                (uc "First")
                (lc "x")
              )
              (unary_double_dot <unary>)
            )
          )
          (lc "rest")
          (malformed malformed:expr_unexpected_token)
        )
      )
      (binop_plus
        (lc "x")
        (apply_anon
          (binop_pipe
            (uc "List")
            (dot_lc "len")
          )
          (lc "rest")
        )
      )
    )
)
  (branch2     (binop_thick_arrow
      (apply_uc
        (uc "Container")
        (block
          (binop_colon
            (lc "items")
            (list_literal)
          )
        )
      )
      (num_literal_i32 0)
    )
)
  (branch3     (binop_thick_arrow
      (apply_uc
        (uc "Wrapper")
        (list_literal
          (apply_uc
            (uc "Tag")
            (lc "value")
          )
          (apply_uc
            (uc "Other")
            (lc "y")
          )
        )
      )
      (binop_plus
        (lc "value")
        (lc "y")
      )
    )
)
  (branch4     (binop_thick_arrow
      (apply_uc
        (uc "Simple")
        (lc "x")
      )
      (lc "x")
    )
))
~~~
# FORMATTED
~~~roc
match data
	Container({
		items : [First(x), ..as ]
		rest
		] 
	}) => x + List.len(rest)
	Container({
		items : []
	}) => 0
	Wrapper([Tag(value), Other(y)]) => value + y
	Simple(x) => x
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:38 to 2:41

**Parse Error**
at 2:24 to 2:41

**Parse Error**
at 2:45 to 2:47

**Unsupported Node**
at 2:50 to 2:52

**Unsupported Node**
at 3:5 to 3:29

**Unsupported Node**
at 4:37 to 4:39

**Unsupported Node**
at 5:5 to 5:14

# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
