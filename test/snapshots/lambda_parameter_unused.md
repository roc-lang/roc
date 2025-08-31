# META
~~~ini
description=Lambda parameters with unused variable checking
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

# Lambda with unused parameter - should warn
add : U64 -> U64
add = |unused| 42

# Lambda with underscore parameter that is used - should warn
multiply : U64 -> U64
multiply = |_factor| _factor * 2

# Lambda with unused underscore parameter - should be fine
process : U64 -> U64
process = |_input| 100

# Lambda with used parameter - should be fine
double : U64 -> U64
double = |value| value * 2

main! = |_| {
    result1 = add(5)
    result2 = multiply(3)
    result3 = process(7)
    result4 = double(4)
    result1 + result2 + result3 + result4
}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar Int BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpStar Int BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar Int BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpStar Int BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }


# Lambda with unused parameter - should warn
add : U64 -> U64
add = |unused| 42
# Lambda with underscore parameter that is used - should warn
multiply : U64 -> U64
multiply = |_factor| _factor * 2
# Lambda with unused underscore parameter - should be fine
process : U64 -> U64
process = |_input| 100
# Lambda with used parameter - should be fine
double : U64 -> U64
double = |value| value * 2
main! = |_| {
	result1 = add(5)
	result2 = multiply(3)
	result3 = process(7)
	result4 = double(4)
	((result1 + result2) + result3) + result4
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **unused** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_unused` to suppress this warning.
The unused variable is declared here:

**lambda_parameter_unused.md:5:8:5:14:**
```roc
add = |unused| 42
```
       ^^^^^^


**UNUSED VARIABLE**
Variable **_input** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `__input` to suppress this warning.
The unused variable is declared here:

**lambda_parameter_unused.md:13:12:13:18:**
```roc
process = |_input| 100
```
           ^^^^^^


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
