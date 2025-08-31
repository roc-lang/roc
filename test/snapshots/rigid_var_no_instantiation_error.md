# META
~~~ini
description=Test showing type error that would occur if rigid variables were not instantiated
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

# Polymorphic function that swaps elements of a tuple
swap : (a, b) -> (b, a)
swap = |pair| {
    (x, y) = pair
    (y, x)
}

# Multiple uses that would conflict if 'a' and 'b' weren't instantiated
main! = |_| {
    # First use: swap (Int, Str)
    result1 = swap((42, "hello"))

    # Second use: swap (Bool, List Int)
    # This would fail if 'a' and 'b' from the first call were reused
    result2 = swap((Bool.true, [1, 2, 3]))

    # Third use: swap (Str, Str)
    # This shows even when both types are the same, we still need fresh vars
    result3 = swap(("foo", "bar"))

    {}
}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LineComment LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly OpenRound LowerIdent Comma LowerIdent CloseRound OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound CloseCurly BlankLine LineComment LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpAssign LowerIdent OpenRound OpenRound Int Comma String CloseRound CloseRound BlankLine LineComment LineComment LowerIdent OpAssign LowerIdent OpenRound OpenRound UpperIdent Dot LowerIdent Comma OpenSquare Int Comma Int Comma Int CloseSquare CloseRound CloseRound BlankLine LineComment LineComment LowerIdent OpAssign LowerIdent OpenRound OpenRound String Comma String CloseRound CloseRound BlankLine OpenCurly CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

# Polymorphic function that swaps elements of a tuple
swap : (a, b) -> (b, a)
swap = |pair| {
	(x, y) = pair((y, x))
}

# Multiple uses that would conflict if 'a' and 'b' weren't instantiated
main! = |_| {
	# First use: swap (Int, Str)
	result1 = swap((42, "hello"))

	# Second use: swap (Bool, List Int)
	# This would fail if 'a' and 'b' from the first call were reused
	result2 = swap((Bool.true, [1, 2, 3]))

	# Third use: swap (Str, Str)
	# This shows even when both types are the same, we still need fresh vars
	result3 = swap(("foo", "bar"))

	{}
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **result1** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result1` to suppress this warning.
The unused variable is declared here:

**rigid_var_no_instantiation_error.md:13:5:13:12:**
```roc
    result1 = swap((42, "hello"))
```
    ^^^^^^^


**UNUSED VARIABLE**
Variable **result2** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result2` to suppress this warning.
The unused variable is declared here:

**rigid_var_no_instantiation_error.md:17:5:17:12:**
```roc
    result2 = swap((Bool.true, [1, 2, 3]))
```
    ^^^^^^^


**UNUSED VARIABLE**
Variable **result3** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result3` to suppress this warning.
The unused variable is declared here:

**rigid_var_no_instantiation_error.md:21:5:21:12:**
```roc
    result3 = swap(("foo", "bar"))
```
    ^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "swap")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "swap"))
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
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
