# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app [main!] { |f: platform "c" }

UserId : U64

ser : UserId -> Str
getUser = |id| if (id > 1!) "big" else "l"

-ain! = |_| getUser(900)
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly OpBar LowerIdent OpColon KwPlatform String CloseCurly UpperIdent OpColon UpperIdent LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwIf OpenRound LowerIdent OpGreaterThan Int OpBang CloseRound String KwElse String OpUnaryMinus LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(block
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (binop_colon
    (uc "UserId")
    (uc "U64")
  )
  (binop_colon
    (lc "ser")
    (binop_thin_arrow
      (uc "UserId")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "getUser")
    (lambda
      (body
        (if_without_else <10 branches>)
      )
      (args
        (lc "id")
      )
    )
  )
  (str_literal_small "big")
  (malformed malformed:expr_unexpected_token)
  (binop_minus
    (str_literal_small "l")
    (lc "ain")
  )
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (underscore)
    )
    (apply_lc
      (lc "getUser")
      (num_literal_i32 900)
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_022.md:1:1:1:4
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_022.md:1:19:1:27
PARSE ERROR - fuzz_crash_022.md:1:28:1:29
PARSE ERROR - fuzz_crash_022.md:1:29:1:30
PARSE ERROR - fuzz_crash_022.md:1:30:1:31
PARSE ERROR - fuzz_crash_022.md:1:32:1:33
PARSE ERROR - fuzz_crash_022.md:6:27:6:28
PARSE ERROR - fuzz_crash_022.md:8:1:8:2
MALFORMED TYPE - fuzz_crash_022.md:1:19:1:27
INVALID IF CONDITION - :0:0:0:0
UNUSED VARIABLE - fuzz_crash_022.md:6:12:6:14
# PROBLEMS
**Expected Package or Platform Name**
at 1:1 to 1:15

**Expected Close Curly Brace**
at 1:1 to 1:15

**No Platform**
at 1:1 to 1:15

**Parse Error**
at 1:19 to 1:19

**Parse Error**
at 1:28 to 1:28

**Parse Error**
at 1:32 to 1:32

**Parse Error**
at 6:26 to 6:26

**Parse Error**
at 6:16 to 6:26

**Parse Error**
at 6:27 to 6:27

**Parse Error**
at 6:35 to 6:35

**Parse Error**
at 8:7 to 8:7

**Unsupported Node**
at 1:28 to 1:28

**Unsupported Node**
at 1:32 to 1:32

**Unsupported Node**
at 5:7 to 5:20

**Unsupported Node**
at 6:11 to 6:16

**Unsupported Node**
at 6:35 to 6:35

**Unsupported Node**
at 8:5 to 8:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.lookup "ser")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.str_literal_small)
  (Expr.malformed)
  (Expr.binop_minus
    (Expr.str_literal_small)
    (Expr.lookup "ain")
  )
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_arg, _arg2 -> _ret")
~~~
# TYPES
~~~roc
~~~
