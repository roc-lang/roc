# META
~~~ini
description=Record destructuring with rest pattern
type=expr
~~~
# SOURCE
~~~roc
match person {
    { first_name, ..others } => Str.len(first_name) > Str.len(others.last_name)
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenCurly LowerIdent Comma DoubleDot LowerIdent CloseCurly OpFatArrow UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound OpGreaterThan UpperIdent Dot LowerIdent OpenRound LowerIdent Dot LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "person")
)
  (branch1     (binop_thick_arrow
      (record_literal
        (lc "first_name")
        (double_dot_lc "others")
      )
      (binop_gt
        (apply_anon
          (binop_pipe
            (uc "Str")
            (dot_lc "len")
          )
          (lc "first_name")
        )
        (apply_anon
          (binop_pipe
            (uc "Str")
            (dot_lc "len")
          )
          (binop_pipe
            (lc "others")
            (dot_lc "last_name")
          )
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
match person
	{first_name, ..others} => Str.len(first_name) > Str.len(others.last_name)
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:30 to 2:32

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
