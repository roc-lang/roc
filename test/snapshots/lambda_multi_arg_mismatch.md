# META
~~~ini
description=Lambda with multiple non-consecutive argument type mismatches
type=file
~~~
# SOURCE
~~~roc
module []

# Function with 8 arguments where several types must match (a appears in positions 1, 3, 5, 7)
multi_arg_fn : a, b, a, c, a, d, a, e -> (a, b, c, d, e)
multi_arg_fn = |x1, x2, x3, x4, x5, x6, x7, x8| 
    (x1, x2, x4, x6, x8)

# Call with mismatched types - args 1, 3, 5, and 7 should all be the same type 'a'
# but we're passing U64, Str, F64, Bool which are all different
result = multi_arg_fn(
    42,        # x1: U64 (type 'a')
    "hello",   # x2: Str (type 'b') - correct
    "world",   # x3: Str (should be 'a' = U64) - MISMATCH  
    1.5,       # x4: F64 (type 'c') - correct
    3.14,      # x5: F64 (should be 'a' = U64) - MISMATCH
    [1, 2],    # x6: List I64 (type 'd') - correct
    True,      # x7: Bool (should be 'a' = U64) - MISMATCH
    "done",    # x8: Str (type 'e') - correct
)
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LineComment LowerIdent OpColon LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent OpArrow OpenRound LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent CloseRound BlankLine LineComment LineComment LowerIdent OpAssign LowerIdent OpenRound Int Comma LineComment String Comma LineComment String Comma LineComment Float Comma LineComment Float Comma LineComment OpenSquare Int Comma Int CloseSquare Comma LineComment UpperIdent Comma LineComment String Comma LineComment CloseRound ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

# Function with 8 arguments where several types must match (a appears in positions 1, 3, 5, 7)
multi_arg_fn : a -> b -> a -> c -> a -> d -> a -> e -> (a, b, c, d, e)
multi_arg_fn = |x1, x2, x3, x4, x5, x6, x7, x8| (x1, x2, x4, x6, x8)

# Call with mismatched types - args 1, 3, 5, and 7 should all be the same type 'a'
# but we're passing U64, Str, F64, Bool which are all different
result = multi_arg_fn((
	42, # x1: U64 (type 'a')
	"hello", # x2: Str (type 'b') - correct
	"world", # x3: Str (should be 'a' = U64) - MISMATCH  
	1.5, # x4: F64 (type 'c') - correct
	3.14, # x5: F64 (should be 'a' = U64) - MISMATCH
	[1, 2], # x6: List I64 (type 'd') - correct
	True, # x7: Bool (should be 'a' = U64) - MISMATCH
	"done",
))

# x8: Str (type 'e') - correct
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **x5** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x5` to suppress this warning.
The unused variable is declared here:

**lambda_multi_arg_mismatch.md:5:33:5:35:**
```roc
multi_arg_fn = |x1, x2, x3, x4, x5, x6, x7, x8| 
```
                                ^^


**UNUSED VARIABLE**
Variable **x3** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x3` to suppress this warning.
The unused variable is declared here:

**lambda_multi_arg_mismatch.md:5:25:5:27:**
```roc
multi_arg_fn = |x1, x2, x3, x4, x5, x6, x7, x8| 
```
                        ^^


**UNUSED VARIABLE**
Variable **x7** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x7` to suppress this warning.
The unused variable is declared here:

**lambda_multi_arg_mismatch.md:5:41:5:43:**
```roc
multi_arg_fn = |x1, x2, x3, x4, x5, x6, x7, x8| 
```
                                        ^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "multi_arg_fn")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "multi_arg_fn"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "result"))
    (Expr.apply_ident)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_f")
~~~
# TYPES
~~~roc
~~~
