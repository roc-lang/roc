# META
~~~ini
description=Simple unused and used underscore variable test
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

# Regular variable that is unused - should warn
unused_regular = |x| 42

# Underscore variable that is used - should warn
used_underscore = |_value| _value

# Underscore variable that is unused - should be fine
unused_underscore = |_ignored| 100

# Regular variable that is used - should be fine
used_regular = |number| number + 1

main! = |_| {
    a = unused_regular(5)
    b = used_underscore(10)
    c = unused_underscore(15)
    d = used_regular(20)
    a + b + c + d
}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar Int BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar Int BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus Int BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseCurly ~~~
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

# Regular variable that is unused - should warn
unused_regular = |x| 42

# Underscore variable that is used - should warn
used_underscore = |_value| _value

# Underscore variable that is unused - should be fine
unused_underscore = |_ignored| 100

# Regular variable that is used - should be fine
used_regular = |number| number + 1

main! = |_| {
	a = unused_regular(5)
	b = used_underscore(10)
	c = unused_underscore(15)
	d = used_regular(20)
	((a + b) + c) + d
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **x** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:

**unused_vars_simple.md:4:19:4:20:**
```roc
unused_regular = |x| 42
```
                  ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "unused_regular"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "used_underscore"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "unused_underscore"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "used_regular"))
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
(expr :tag block :type "_e")
~~~
# TYPES
~~~roc
~~~
