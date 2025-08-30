# META
~~~ini
description=Return statement in a block context
type=file
~~~
# SOURCE
~~~roc
module [foo]

foo : U64 -> Result(Str, [TooBig])
foo = |num| {
    str = if (num > 10) {
        return Err(TooBig)
    } else {
        "SMALL"
    }
    Ok(str)
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpColon UpperIdent OpArrow UpperIdent OpenRound UpperIdent Comma OpenSquare UpperIdent CloseSquare CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign KwIf OpenRound LowerIdent OpGreaterThan Int CloseRound OpenCurly KwReturn UpperIdent OpenRound UpperIdent CloseRound CloseCurly KwElse OpenCurly String CloseCurly UpperIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "foo")
))
~~~
# FORMATTED
~~~roc
module [foo]

foo : U64 -> Result(Str, [TooBig])
foo = |num| {
	str = if num > 10
		{
			return Err(TooBig)
		}
	else {
		"SMALL"
	}
	Ok(str)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "foo")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "foo")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
foo : _a
~~~
