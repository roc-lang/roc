# META
~~~ini
description=Simple demonstration that type variable names avoid collision with existing identifiers
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

# Define some variables that would normally be used for type variables
a = 1
b = 2
c = 3

# This identity function should get type 'd -> d' since a, b, c are taken
identity = |x| x

# This function should get type 'e -> e' since d is now also taken
identity2 = |y| y

# This function with two parameters should get types 'f, g -> (f, g)'
pair = |first, second| (first, second)

main! = |_| {
    result1 = identity(42)
    result2 = identity2("hello")
    result3 = pair(result1, result2)
    
    a + b + c
}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LineComment LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseCurly ~~~
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

# Define some variables that would normally be used for type variables
a = 1
b = 2
c = 3

# This identity function should get type 'd -> d' since a, b, c are taken
identity = |x| x

# This function should get type 'e -> e' since d is now also taken
identity2 = |y| y

# This function with two parameters should get types 'f, g -> (f, g)'
pair = |first, second| (first, second)

main! = |_| {
	result1 = identity(42)
	result2 = identity2("hello")
	result3 = pair((result1, result2))

	(a + b) + c
}
~~~
# EXPECTED
UNUSED VARIABLE - type_var_collision_simple.md:20:5:20:12
# PROBLEMS
**UNUSED VARIABLE**
Variable **result3** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result3` to suppress this warning.
The unused variable is declared here:

**type_var_collision_simple.md:20:5:20:12:**
```roc
    result3 = pair(result1, result2)
```
    ^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "a"))
    (Expr.num_literal_i32 1)
  )
  (Stmt.assign
    (pattern (Patt.ident "b"))
    (Expr.num_literal_i32 2)
  )
  (Stmt.assign
    (pattern (Patt.ident "c"))
    (Expr.num_literal_i32 3)
  )
  (Stmt.assign
    (pattern (Patt.ident "identity"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "identity2"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "pair"))
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
