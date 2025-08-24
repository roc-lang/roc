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
(block
  (binop_colon
    (lc "foo")
    (binop_thin_arrow
      (uc "U64")
      (apply_uc
        (uc "Result")
        (tuple_literal
          (uc "Str")
          (list_literal
            (uc "TooBig")
          )
        )
      )
    )
  )
  (binop_equals
    (lc "foo")
    (lambda
      (body
        (block
          (binop_equals
            (lc "str")
            (if_else <12 branches>)
          )
          (apply_uc
            (uc "Ok")
            (lc "str")
          )
        )
      )
      (args
        (lc "num")
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
INCOMPATIBLE IF BRANCHES - return_stmt_block_example.md:5:11:5:11
# PROBLEMS
**Parse Error**
at 5:11 to 5:25

**Unsupported Node**
at 3:7 to 4:1

**Unsupported Node**
at 4:7 to 4:13

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "foo")
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
