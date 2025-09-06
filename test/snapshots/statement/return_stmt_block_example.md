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
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent OpenRound UpperIdent Comma OpenSquare UpperIdent CloseSquare CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign KwIf OpenRound LowerIdent OpGreaterThan Int CloseRound OpenCurly KwReturn UpperIdent OpenRound UpperIdent CloseRound CloseCurly KwElse OpenCurly String CloseCurly UpperIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "foo")
))
(block
  (binop_colon
    (lc "foo")
    (binop_arrow_call
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
            (if_else
              (condition                 (binop_gt
                  (lc "num")
                  (num_literal_i32 10)
                )
)
              (then                 (block
                  (ret
                    (apply_uc
                      (uc "Err")
                      (uc "TooBig")
                    )
                  )
                )
)
              (else                 (block
                  (str_literal_big "SMALL")
                )
))
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
INCOMPATIBLE IF BRANCHES - return_stmt_block_example.md:5:11:5:11
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "foo"))
    (type type_10)
  )
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 37
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 -> #36)
(var #13 _)
(var #14 -> #25)
(var #15 _)
(var #16 Num *)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 Str)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 -> #35)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 -> #36)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 fn_pure)
(var #36 fn_pure)
~~~
# TYPES
~~~roc
foo : _arg -> _ret
num : _a
str : _a
~~~
