# META
~~~ini
description=Basic tag union type canonicalization
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

process : [Some(Str), None] -> Str
process = |maybe| "result"

is_ok_ret_unqualified_bool : [Ok(_ok), Err(_err)] -> Bool
is_ok_ret_unqualified_bool = |result| match result {
    Ok(_) => True
    Err(_) => False
}

is_ok_ret_bool : [Ok(_ok2), Err(_err2)] -> Bool
is_ok_ret_bool = |result| match result {
    Ok(_) => Bool.True
    Err(_) => Bool.False
}

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon OpenSquare UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent CloseSquare OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String LowerIdent OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound Underscore CloseRound OpFatArrow UpperIdent UpperIdent OpenRound Underscore CloseRound OpFatArrow UpperIdent CloseCurly LowerIdent OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound Underscore CloseRound OpFatArrow UpperIdent Dot UpperIdent UpperIdent OpenRound Underscore CloseRound OpFatArrow UpperIdent Dot UpperIdent CloseCurly LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "process")
    (binop_thin_arrow
      (list_literal
        (apply_uc
          (uc "Some")
          (uc "Str")
        )
        (uc "None")
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "process")
    (lambda
      (body
        (str_literal_big "result")
      )
      (args
        (lc "maybe")
      )
    )
  )
  (binop_colon
    (lc "is_ok_ret_unqualified_bool")
    (binop_thin_arrow
      (list_literal
        (apply_uc
          (uc "Ok")
          (lc "_ok")
        )
        (apply_uc
          (uc "Err")
          (lc "_err")
        )
      )
      (uc "Bool")
    )
  )
  (binop_equals
    (lc "is_ok_ret_unqualified_bool")
    (lambda
      (body
        (match
          (scrutinee             (lc "result")
))
      )
      (args
        (lc "result")
      )
    )
  )
  (binop_colon
    (lc "is_ok_ret_bool")
    (binop_thin_arrow
      (list_literal
        (apply_uc
          (uc "Ok")
          (lc "_ok2")
        )
        (apply_uc
          (uc "Err")
          (lc "_err2")
        )
      )
      (uc "Bool")
    )
  )
  (binop_equals
    (lc "is_ok_ret_bool")
    (lambda
      (body
        (match
          (scrutinee             (lc "result")
))
      )
      (args
        (lc "result")
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
	pf: "../basic-cli/main.roc" platform [main],
}

process : [Some(Str), None] -> Str
process = \maybe -> "result"
is_ok_ret_unqualified_bool : [Ok(_ok), Err(_err)] -> Bool
is_ok_ret_unqualified_bool = \result -> match result
is_ok_ret_bool : [Ok(_ok2), Err(_err2)] -> Bool
is_ok_ret_bool = \result -> match result
main! = \_ -> {  }
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 8:11 to 8:11

**Parse Error**
at 9:12 to 9:12

**Parse Error**
at 14:11 to 14:11

**Parse Error**
at 15:12 to 15:12

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
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
