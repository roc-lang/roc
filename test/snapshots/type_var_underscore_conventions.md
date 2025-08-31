# META
~~~ini
description=Comprehensive test of type variable underscore conventions
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }

# Test 1: UNUSED TYPE VARIABLE NAME - single-use variable should start with underscore
single_use : List(elem) -> Str
single_use = |x| "hello"

# Test 2: TYPE VAR ENDING IN UNDERSCORE - variables should never end with underscore
ending_underscore : List(elem_) -> elem_
ending_underscore = |list| "default"

# Test 3: COMBINATION - single-use ending in underscore (both errors)
combo_single : List(bad_) -> Str
combo_single = |x| "combo"

# Test 4: VALID CASES - these should not generate warnings
valid_single : List(_elem) -> Str
valid_single = |x| "valid"

valid_multi : elem -> List(elem)
valid_multi = |x| [x]

main = |x| "done"
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent CloseSquare CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LowerIdent OpColon LowerIdent OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpAssign OpBar LowerIdent OpBar String ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block
          (lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }


# Test 1: UNUSED TYPE VARIABLE NAME - single-use variable should start with underscore
single_use : List elem -> Str
single_use = |x| "hello"
# Test 2: TYPE VAR ENDING IN UNDERSCORE - variables should never end with underscore
ending_underscore : List elem_ -> elem_
ending_underscore = |list| "default"
# Test 3: COMBINATION - single-use ending in underscore (both errors)
combo_single : List bad_ -> Str
combo_single = |x| "combo"
# Test 4: VALID CASES - these should not generate warnings
valid_single : List _elem -> Str
valid_single = |x| "valid"
valid_multi : elem -> List elem
valid_multi = |x| [x]
main = |x| "done"
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **x** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:

**type_var_underscore_conventions.md:5:15:5:16:**
```roc
single_use = |x| "hello"
```
              ^


**UNUSED VARIABLE**
Variable **list** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_list` to suppress this warning.
The unused variable is declared here:

**type_var_underscore_conventions.md:9:22:9:26:**
```roc
ending_underscore = |list| "default"
```
                     ^^^^


**UNUSED VARIABLE**
Variable **x** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:

**type_var_underscore_conventions.md:13:17:13:18:**
```roc
combo_single = |x| "combo"
```
                ^


**UNUSED VARIABLE**
Variable **x** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:

**type_var_underscore_conventions.md:17:17:17:18:**
```roc
valid_single = |x| "valid"
```
                ^


**UNUSED VARIABLE**
Variable **x** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:

**type_var_underscore_conventions.md:22:9:22:10:**
```roc
main = |x| "done"
```
        ^


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
