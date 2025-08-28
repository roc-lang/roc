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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent CloseSquare CloseCurly LowerIdent OpAssign Int LowerIdent OpAssign Float LowerIdent OpAssign LowerIdent LowerIdent OpAssign LowerIdent LowerIdent OpAssign LowerIdent OpPlus Int LowerIdent OpAssign LowerIdent OpStar Int LowerIdent OpAssign LowerIdent OpPlus Float LowerIdent OpAssign LowerIdent OpStar Float LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpStar Int LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Float CloseRound LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "num")
    (num_literal_i32 42)
  )
  (binop_equals
    (lc "frac")
    (frac_literal_small 4.2)
  )
  (binop_equals
    (lc "int_use")
    (lc "num")
  )
  (binop_equals
    (lc "float_use")
    (lc "frac")
  )
  (binop_equals
    (lc "int_add")
    (binop_plus
      (lc "num")
      (num_literal_i32 10)
    )
  )
  (binop_equals
    (lc "int_multiply")
    (binop_star
      (lc "num")
      (num_literal_i32 2)
    )
  )
  (binop_equals
    (lc "float_add")
    (binop_plus
      (lc "num")
      (frac_literal_small 3.14)
    )
  )
  (binop_equals
    (lc "float_multiply")
    (binop_star
      (lc "num")
      (frac_literal_small 2.5)
    )
  )
  (binop_equals
    (lc "double")
    (lambda
      (body
        (binop_star
          (lc "x")
          (num_literal_i32 2)
        )
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_equals
    (lc "int_doubled")
    (apply_lc
      (lc "double")
      (num_literal_i32 5)
    )
  )
  (binop_equals
    (lc "float_doubled")
    (apply_lc
      (lc "double")
      (frac_literal_small 2.5)
    )
  )
  (binop_equals
    (lc "main")
    (lambda
      (body
        (block
          (binop_plus
            (lc "int_add")
            (lc "int_multiply")
          )
        )
      )
      (args
        (underscore)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }

num = 42
frac = 4.2
int_use = num
float_use = frac
int_add = num + 10
int_multiply = num * 2
float_add = num + 3.14
float_multiply = num * 2.5
double = |x| x * 2
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
  (Expr.binop_equals
    (Expr.lookup "num")
    (Expr.num_literal_i32 42)
  )
  (Expr.binop_equals
    (Expr.lookup "frac")
    (Expr.frac_literal_small 4.2)
  )
  (Expr.binop_equals
    (Expr.lookup "int_use")
    (Expr.lookup "num")
  )
  (Expr.binop_equals
    (Expr.lookup "float_use")
    (Expr.lookup "frac")
  )
  (Expr.binop_equals
    (Expr.lookup "int_add")
    (Expr.binop_plus
      (Expr.lookup "num")
      (Expr.num_literal_i32 10)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "int_multiply")
    (Expr.binop_star
      (Expr.lookup "num")
      (Expr.num_literal_i32 2)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "float_add")
    (Expr.binop_plus
      (Expr.lookup "num")
      (Expr.frac_literal_small 3.14)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "float_multiply")
    (Expr.binop_star
      (Expr.lookup "num")
      (Expr.frac_literal_small 2.5)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "double")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "int_doubled")
    (Expr.apply_ident)
  )
  (Expr.binop_equals
    (Expr.lookup "float_doubled")
    (Expr.apply_ident)
  )
  (Expr.binop_equals
    (Expr.lookup "main")
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
num : Num(_size)
frac : F64
int_use : _a
float_use : _a
int_add : Num(_size)
int_multiply : Num(_size)
float_add : Num(_size)
float_multiply : Num(_size)
double : _a
int_doubled : _a
float_doubled : _a
main : _a
~~~
