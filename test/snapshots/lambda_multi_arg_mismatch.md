# META
~~~ini
description=Lambda with multiple non-consecutive argument type mismatches
type=snippet
~~~
# SOURCE
~~~roc
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
# EXPECTED
UNUSED VARIABLE - lambda_multi_arg_mismatch.md:3:25:3:27
UNUSED VARIABLE - lambda_multi_arg_mismatch.md:3:33:3:35
UNUSED VARIABLE - lambda_multi_arg_mismatch.md:3:41:3:43
TYPE MISMATCH - lambda_multi_arg_mismatch.md:9:5:9:5
# PROBLEMS
**UNUSED VARIABLE**
Variable `x3` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x3` to suppress this warning.
The unused variable is declared here:
**lambda_multi_arg_mismatch.md:3:25:3:27:**
```roc
multi_arg_fn = |x1, x2, x3, x4, x5, x6, x7, x8| 
```
                        ^^


**UNUSED VARIABLE**
Variable `x5` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x5` to suppress this warning.
The unused variable is declared here:
**lambda_multi_arg_mismatch.md:3:33:3:35:**
```roc
multi_arg_fn = |x1, x2, x3, x4, x5, x6, x7, x8| 
```
                                ^^


**UNUSED VARIABLE**
Variable `x7` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x7` to suppress this warning.
The unused variable is declared here:
**lambda_multi_arg_mismatch.md:3:41:3:43:**
```roc
multi_arg_fn = |x1, x2, x3, x4, x5, x6, x7, x8| 
```
                                        ^^


**TYPE MISMATCH**
The first and third arguments to `multi_arg_fn` must have compatible types, but they are incompatible in this call:
**lambda_multi_arg_mismatch.md:9:5:**
```roc
    42,        # x1: U64 (type 'a')
    "hello",   # x2: Str (type 'b') - correct
    "world",   # x3: Str (should be 'a' = U64) - MISMATCH  
```
    ^^
    ^^^^^^^

The first argument has the type:
    _Num(_size)_

But the third argument has the type:
    

`multi_arg_fn` needs these arguments to have compatible types.

# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpArrow,OpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBar,
OpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,
Int,Comma,
StringStart,StringPart,StringEnd,Comma,
StringStart,StringPart,StringEnd,Comma,
Float,Comma,
Float,Comma,
OpenSquare,Int,Comma,Int,CloseSquare,Comma,
UpperIdent,Comma,
StringStart,StringPart,StringEnd,Comma,
CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "multi_arg_fn")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b"))
				(ty-var (raw "a"))
				(ty-var (raw "c"))
				(ty-var (raw "a"))
				(ty-var (raw "d"))
				(ty-var (raw "a"))
				(ty-var (raw "e"))
				(ty-tuple
					(ty-var (raw "a"))
					(ty-var (raw "b"))
					(ty-var (raw "c"))
					(ty-var (raw "d"))
					(ty-var (raw "e")))))
		(s-decl
			(p-ident (raw "multi_arg_fn"))
			(e-lambda
				(args
					(p-ident (raw "x1"))
					(p-ident (raw "x2"))
					(p-ident (raw "x3"))
					(p-ident (raw "x4"))
					(p-ident (raw "x5"))
					(p-ident (raw "x6"))
					(p-ident (raw "x7"))
					(p-ident (raw "x8")))
				(e-tuple
					(e-ident (raw "x1"))
					(e-ident (raw "x2"))
					(e-ident (raw "x4"))
					(e-ident (raw "x6"))
					(e-ident (raw "x8")))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "multi_arg_fn"))
				(e-int (raw "42"))
				(e-string
					(e-string-part (raw "hello")))
				(e-string
					(e-string-part (raw "world")))
				(e-frac (raw "1.5"))
				(e-frac (raw "3.14"))
				(e-list
					(e-int (raw "1"))
					(e-int (raw "2")))
				(e-tag (raw "True"))
				(e-string
					(e-string-part (raw "done")))))))
~~~
# FORMATTED
~~~roc
# Function with 8 arguments where several types must match (a appears in positions 1, 3, 5, 7)
multi_arg_fn : a, b, a, c, a, d, a, e -> (a, b, c, d, e)
multi_arg_fn = |x1, x2, x3, x4, x5, x6, x7, x8|
	(x1, x2, x4, x6, x8)

# Call with mismatched types - args 1, 3, 5, and 7 should all be the same type 'a'
# but we're passing U64, Str, F64, Bool which are all different
result = multi_arg_fn(
	42, # x1: U64 (type 'a')
	"hello", # x2: Str (type 'b') - correct
	"world", # x3: Str (should be 'a' = U64) - MISMATCH  
	1.5, # x4: F64 (type 'c') - correct
	3.14, # x5: F64 (should be 'a' = U64) - MISMATCH
	[1, 2], # x6: List I64 (type 'd') - correct
	True, # x7: Bool (should be 'a' = U64) - MISMATCH
	"done", # x8: Str (type 'e') - correct
)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "multi_arg_fn"))
		(e-lambda
			(args
				(p-assign (ident "x1"))
				(p-assign (ident "x2"))
				(p-assign (ident "x3"))
				(p-assign (ident "x4"))
				(p-assign (ident "x5"))
				(p-assign (ident "x6"))
				(p-assign (ident "x7"))
				(p-assign (ident "x8")))
			(e-tuple
				(elems
					(e-lookup-local
						(p-assign (ident "x1")))
					(e-lookup-local
						(p-assign (ident "x2")))
					(e-lookup-local
						(p-assign (ident "x4")))
					(e-lookup-local
						(p-assign (ident "x6")))
					(e-lookup-local
						(p-assign (ident "x8"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))
				(ty-rigid-var-lookup (ty-rigid-var (name "a")))
				(ty-rigid-var (name "c"))
				(ty-rigid-var-lookup (ty-rigid-var (name "a")))
				(ty-rigid-var (name "d"))
				(ty-rigid-var-lookup (ty-rigid-var (name "a")))
				(ty-rigid-var (name "e"))
				(ty-tuple
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
					(ty-rigid-var-lookup (ty-rigid-var (name "b")))
					(ty-rigid-var-lookup (ty-rigid-var (name "c")))
					(ty-rigid-var-lookup (ty-rigid-var (name "d")))
					(ty-rigid-var-lookup (ty-rigid-var (name "e")))))))
	(d-let
		(p-assign (ident "result"))
		(e-call
			(e-lookup-local
				(p-assign (ident "multi_arg_fn")))
			(e-num (value "42"))
			(e-string
				(e-literal (string "hello")))
			(e-string
				(e-literal (string "world")))
			(e-dec-small (numerator "15") (denominator-power-of-ten "1") (value "1.5"))
			(e-dec-small (numerator "314") (denominator-power-of-ten "2") (value "3.14"))
			(e-list
				(elems
					(e-num (value "1"))
					(e-num (value "2"))))
			(e-tag (name "True"))
			(e-string
				(e-literal (string "done"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a, b, a, c, a, d, a, e -> (a, b, c, d, e)"))
		(patt (type "Error")))
	(expressions
		(expr (type "a, b, a, c, a, d, a, e -> (a, b, c, d, e)"))
		(expr (type "Error"))))
~~~
