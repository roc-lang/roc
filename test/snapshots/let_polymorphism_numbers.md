# META
~~~ini
description=Let-polymorphism with numbers
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }

# Basic number polymorphism
num = 42
frac = 4.2

# Using polymorphic values in different contexts
int_use = num
float_use = frac

# num used as Int
int_add = num + 10
int_multiply = num * 2

# num used as Float
float_add = num + 3.14
float_multiply = num * 2.5

# Polymorphic function with numeric types
double = |x| x * 2

# Used with different numeric types
int_doubled = double(5)
float_doubled = double(2.5)

main = |_| {
    # Combine results
    int_add + int_multiply
}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent CloseSquare CloseCurly BlankLine LineComment LowerIdent OpAssign Int LowerIdent OpAssign Float BlankLine LineComment LowerIdent OpAssign LowerIdent LowerIdent OpAssign LowerIdent BlankLine LineComment LowerIdent OpAssign LowerIdent OpPlus Int LowerIdent OpAssign LowerIdent OpStar Int BlankLine LineComment LowerIdent OpAssign LowerIdent OpPlus Float LowerIdent OpAssign LowerIdent OpStar Float BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpStar Int BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Float CloseRound BlankLine LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpPlus LowerIdent CloseCurly ~~~
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

# Basic number polymorphism
num = 42
frac = 4.2

# Using polymorphic values in different contexts
int_use = num
float_use = frac

# num used as Int
int_add = num + 10
int_multiply = num * 2

# num used as Float
float_add = num + 3.14
float_multiply = num * 2.5

# Polymorphic function with numeric types
double = |x| x * 2

# Used with different numeric types
int_doubled = double(5)
float_doubled = double(2.5)

main = |_| {
	# Combine results
	int_add + int_multiply
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "num"))
    (Expr.num_literal_i32 42)
  )
  (Stmt.assign
    (pattern (Patt.ident "frac"))
    (Expr.frac_literal_small 4.2)
  )
  (Stmt.assign
    (pattern (Patt.ident "int_use"))
    (Expr.lookup "num")
  )
  (Stmt.assign
    (pattern (Patt.ident "float_use"))
    (Expr.lookup "frac")
  )
  (Stmt.assign
    (pattern (Patt.ident "int_add"))
    (Expr.binop_plus
      (Expr.lookup "num")
      (Expr.num_literal_i32 10)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "int_multiply"))
    (Expr.binop_star
      (Expr.lookup "num")
      (Expr.num_literal_i32 2)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "float_add"))
    (Expr.binop_plus
      (Expr.lookup "num")
      (Expr.frac_literal_small 3.14)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "float_multiply"))
    (Expr.binop_star
      (Expr.lookup "num")
      (Expr.frac_literal_small 2.5)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "double"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "int_doubled"))
    (Expr.apply_ident)
  )
  (Stmt.assign
    (pattern (Patt.ident "float_doubled"))
    (Expr.apply_ident)
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
