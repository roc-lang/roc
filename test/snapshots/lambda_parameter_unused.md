# META
~~~ini
description=Lambda parameters with unused variable checking
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

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
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar Int BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpStar Int BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar Int BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpStar Int BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (not_lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block)
      )
    )
))
~~~
# FORMATTED
~~~roc
app [main!] { pf: "../basic-cli/main.roc" platform [] }

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
UNUSED VARIABLE - lambda_parameter_unused.md:5:8:5:14
UNDERSCORE VARIABLE USED - lambda_parameter_unused.md:9:22:9:29
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


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "add")
    (type <mutated_tag:161>)
  )
  (Stmt.assign
    (pattern (Patt.ident "add"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "multiply")
    (type <mutated_tag:161>)
  )
  (Stmt.assign
    (pattern (Patt.ident "multiply"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "process")
    (type <mutated_tag:161>)
  )
  (Stmt.assign
    (pattern (Patt.ident "process"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "double")
    (type <mutated_tag:161>)
  )
  (Stmt.assign
    (pattern (Patt.ident "double"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
