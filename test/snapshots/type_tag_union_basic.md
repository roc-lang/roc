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
        (binop_thin_arrow
          (binop_colon
            (match
              (scrutinee                 (lc "result")
))
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
          )
          (uc "Bool")
        )
      )
      (args
        (lc "result")
      )
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
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (underscore)
    )
    (record_literal)
  )
)
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main] }

process : [Some(Str), None] -> Str
process = |maybe| "result"
is_ok_ret_unqualified_bool : [Ok(_ok), Err(_err)] -> Bool
is_ok_ret_unqualified_bool = |result| match result
 : [Ok(_ok2), Err(_err2)] -> Bool
is_ok_ret_bool = |result| match result
(!=  | _) | {  }
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 8:11 to 8:14

**Parse Error**
at 9:12 to 9:15

**Parse Error**
at 7:52 to 12:16

**Parse Error**
at 14:11 to 14:14

**Parse Error**
at 15:12 to 15:15

**Parse Error**
at 13:40 to 18:5

**Parse Error**
at 18:7 to 18:9

**Unsupported Node**
at 18:5 to 18:6

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "process")
    (Expr.binop_thin_arrow
      (Expr.list_literal)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "process")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "is_ok_ret_unqualified_bool")
    (Expr.binop_thin_arrow
      (Expr.list_literal)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "is_ok_ret_unqualified_bool")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "is_ok_ret_bool")
    (Expr.lambda)
  )
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
process : _a
is_ok_ret_unqualified_bool : _a
is_ok_ret_bool : _a
~~~
