# META
~~~ini
description=Basic tag union type canonicalization
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

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
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly LowerIdent OpColon OpenSquare UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent CloseSquare OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String LowerIdent OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound Underscore CloseRound OpFatArrow UpperIdent UpperIdent OpenRound Underscore CloseRound OpFatArrow UpperIdent CloseCurly LowerIdent OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound Underscore CloseRound OpFatArrow UpperIdent Dot UpperIdent UpperIdent OpenRound Underscore CloseRound OpFatArrow UpperIdent Dot UpperIdent CloseCurly LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "process")
    (binop_thin_arrow
      (list_literal
        (tuple_literal
          (apply_uc
            (uc "Some")
            (uc "Str")
          )
          (uc "None")
        )
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
        (tuple_literal
          (apply_uc
            (uc "Ok")
            (lc "_ok")
          )
          (apply_uc
            (uc "Err")
            (lc "_err")
          )
        )
      )
      (uc "Bool")
    )
  )
  (binop_equals
    (lc "is_ok_ret_unqualified_bool")
    (lambda
      (body
        (match <44 branches>)
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
        (match <65 branches>)
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
NO CHANGE
~~~
# EXPECTED
UNUSED VARIABLE - type_tag_union_basic.md:4:12:4:17
INCOMPATIBLE MATCH PATTERNS - type_tag_union_basic.md:7:39:7:39
INCOMPATIBLE MATCH PATTERNS - type_tag_union_basic.md:13:27:13:27
# PROBLEMS
**Parse Error**
at 7:39 to 7:52

**Parse Error**
at 8:11 to 8:11

**Parse Error**
at 9:12 to 9:12

**Parse Error**
at 7:39 to 12:1

**Parse Error**
at 13:27 to 13:40

**Parse Error**
at 14:11 to 14:11

**Parse Error**
at 15:12 to 15:12

**Parse Error**
at 13:27 to 18:1

**Parse Error**
at 18:7 to 18:7

**Unsupported Node**
at 3:11 to 3:35

**Unsupported Node**
at 4:11 to 4:19

**Unsupported Node**
at 6:30 to 6:58

**Unsupported Node**
at 7:30 to 7:39

**Unsupported Node**
at 13:18 to 13:27

**Unsupported Node**
at 18:5 to 18:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "process")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "is_ok_ret_unqualified_bool")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
