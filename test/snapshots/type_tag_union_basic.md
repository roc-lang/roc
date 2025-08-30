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
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block
          (lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main] }

process : [Some(Str), None] -> Str
process = |maybe| "result"
is_ok_ret_unqualified_bool : [Ok(_ok), Err(_err)] -> Bool
is_ok_ret_unqualified_bool = |result| match result
	Ok(_) => True
	Err(_) => False
is_ok_ret_bool : [Ok(_ok2), Err(_err2)] -> Bool
is_ok_ret_bool = |result| match result
	Ok(_) => Bool.True
	Err(_) => Bool.False
main! = |_| {  }
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 8:11 to 8:13

**Unsupported Node**
at 9:5 to 9:11

**Unsupported Node**
at 14:11 to 14:13

**Unsupported Node**
at 15:5 to 15:11

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
  (Expr.binop_colon
    (Expr.lookup "is_ok_ret_bool")
    (Expr.binop_thin_arrow
      (Expr.list_literal)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "is_ok_ret_bool")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.not_lookup)
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
process : _a
is_ok_ret_unqualified_bool : _a
is_ok_ret_bool : _a
~~~
