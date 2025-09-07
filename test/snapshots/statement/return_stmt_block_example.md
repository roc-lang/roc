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
**SHADOWING**
This definition shadows an existing one.

**return_stmt_block_example.md:4:1:4:4:**
```roc
foo = |num| {
```
^^^


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
; Total type variables: 39
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
(var #12 -> #38)
(var #13 _)
(var #14 -> #24)
(var #15 -> #16)
(var #16 -> #17)
(var #17 -> #35)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 -> #24)
(var #23 Str)
(var #24 _)
(var #25 -> #24)
(var #26 _)
(var #27 -> #37)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 -> #38)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 Num *)
(var #36 _)
(var #37 fn_pure)
(var #38 fn_pure)
~~~
# TYPES
~~~roc
foo : _arg -> _ret
num : _a
~~~
