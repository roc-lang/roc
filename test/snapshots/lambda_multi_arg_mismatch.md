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
# EXPECTED
UNUSED VARIABLE - lambda_multi_arg_mismatch.md:5:25:5:27
UNUSED VARIABLE - lambda_multi_arg_mismatch.md:5:33:5:35
UNUSED VARIABLE - lambda_multi_arg_mismatch.md:5:41:5:43
TYPE MISMATCH - lambda_multi_arg_mismatch.md:11:5:11:5
# PROBLEMS
**UNUSED VARIABLE**
Variable `x3` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x3` to suppress this warning.
The unused variable is declared here:
**lambda_multi_arg_mismatch.md:5:25:5:27:**
```roc
multi_arg_fn = |x1, x2, x3, x4, x5, x6, x7, x8| 
```
                        ^^


**UNUSED VARIABLE**
Variable `x5` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x5` to suppress this warning.
The unused variable is declared here:
**lambda_multi_arg_mismatch.md:5:33:5:35:**
```roc
multi_arg_fn = |x1, x2, x3, x4, x5, x6, x7, x8| 
```
                                ^^


**UNUSED VARIABLE**
Variable `x7` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x7` to suppress this warning.
The unused variable is declared here:
**lambda_multi_arg_mismatch.md:5:41:5:43:**
```roc
multi_arg_fn = |x1, x2, x3, x4, x5, x6, x7, x8| 
```
                                        ^^


**TYPE MISMATCH**
The first and third arguments to `multi_arg_fn` must have compatible types, but they are incompatible in this call:
**lambda_multi_arg_mismatch.md:11:5:**
```roc
    42,        # x1: U64 (type 'a')
    "hello",   # x2: Str (type 'b') - correct
    "world",   # x3: Str (should be 'a' = U64) - MISMATCH  
```
    ^^
    ^^^^^^^

The first argument is of type:
    _Num(_size)_

But the third argument is of type:
    _Str_

`multi_arg_fn` needs these arguments to have compatible types.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
LowerIdent(4:1-4:13),OpColon(4:14-4:15),LowerIdent(4:16-4:17),Comma(4:17-4:18),LowerIdent(4:19-4:20),Comma(4:20-4:21),LowerIdent(4:22-4:23),Comma(4:23-4:24),LowerIdent(4:25-4:26),Comma(4:26-4:27),LowerIdent(4:28-4:29),Comma(4:29-4:30),LowerIdent(4:31-4:32),Comma(4:32-4:33),LowerIdent(4:34-4:35),Comma(4:35-4:36),LowerIdent(4:37-4:38),OpArrow(4:39-4:41),OpenRound(4:42-4:43),LowerIdent(4:43-4:44),Comma(4:44-4:45),LowerIdent(4:46-4:47),Comma(4:47-4:48),LowerIdent(4:49-4:50),Comma(4:50-4:51),LowerIdent(4:52-4:53),Comma(4:53-4:54),LowerIdent(4:55-4:56),CloseRound(4:56-4:57),
LowerIdent(5:1-5:13),OpAssign(5:14-5:15),OpBar(5:16-5:17),LowerIdent(5:17-5:19),Comma(5:19-5:20),LowerIdent(5:21-5:23),Comma(5:23-5:24),LowerIdent(5:25-5:27),Comma(5:27-5:28),LowerIdent(5:29-5:31),Comma(5:31-5:32),LowerIdent(5:33-5:35),Comma(5:35-5:36),LowerIdent(5:37-5:39),Comma(5:39-5:40),LowerIdent(5:41-5:43),Comma(5:43-5:44),LowerIdent(5:45-5:47),OpBar(5:47-5:48),
OpenRound(6:5-6:6),LowerIdent(6:6-6:8),Comma(6:8-6:9),LowerIdent(6:10-6:12),Comma(6:12-6:13),LowerIdent(6:14-6:16),Comma(6:16-6:17),LowerIdent(6:18-6:20),Comma(6:20-6:21),LowerIdent(6:22-6:24),CloseRound(6:24-6:25),
LowerIdent(10:1-10:7),OpAssign(10:8-10:9),LowerIdent(10:10-10:22),NoSpaceOpenRound(10:22-10:23),
Int(11:5-11:7),Comma(11:7-11:8),
StringStart(12:5-12:6),StringPart(12:6-12:11),StringEnd(12:11-12:12),Comma(12:12-12:13),
StringStart(13:5-13:6),StringPart(13:6-13:11),StringEnd(13:11-13:12),Comma(13:12-13:13),
Float(14:5-14:8),Comma(14:8-14:9),
Float(15:5-15:9),Comma(15:9-15:10),
OpenSquare(16:5-16:6),Int(16:6-16:7),Comma(16:7-16:8),Int(16:9-16:10),CloseSquare(16:10-16:11),Comma(16:11-16:12),
UpperIdent(17:5-17:9),Comma(17:9-17:10),
StringStart(18:5-18:6),StringPart(18:6-18:10),StringEnd(18:10-18:11),Comma(18:11-18:12),
CloseRound(19:1-19:2),EndOfFile(19:2-19:2),
~~~
# PARSE
~~~clojure
(file @1.1-19.2
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-anno @4.1-4.57 (name "multi_arg_fn")
			(ty-fn @4.16-4.57
				(ty-var @4.16-4.17 (raw "a"))
				(ty-var @4.19-4.20 (raw "b"))
				(ty-var @4.22-4.23 (raw "a"))
				(ty-var @4.25-4.26 (raw "c"))
				(ty-var @4.28-4.29 (raw "a"))
				(ty-var @4.31-4.32 (raw "d"))
				(ty-var @4.34-4.35 (raw "a"))
				(ty-var @4.37-4.38 (raw "e"))
				(ty-tuple @4.42-4.57
					(ty-var @4.43-4.44 (raw "a"))
					(ty-var @4.46-4.47 (raw "b"))
					(ty-var @4.49-4.50 (raw "c"))
					(ty-var @4.52-4.53 (raw "d"))
					(ty-var @4.55-4.56 (raw "e")))))
		(s-decl @5.1-6.25
			(p-ident @5.1-5.13 (raw "multi_arg_fn"))
			(e-lambda @5.16-6.25
				(args
					(p-ident @5.17-5.19 (raw "x1"))
					(p-ident @5.21-5.23 (raw "x2"))
					(p-ident @5.25-5.27 (raw "x3"))
					(p-ident @5.29-5.31 (raw "x4"))
					(p-ident @5.33-5.35 (raw "x5"))
					(p-ident @5.37-5.39 (raw "x6"))
					(p-ident @5.41-5.43 (raw "x7"))
					(p-ident @5.45-5.47 (raw "x8")))
				(e-tuple @6.5-6.25
					(e-ident @6.6-6.8 (raw "x1"))
					(e-ident @6.10-6.12 (raw "x2"))
					(e-ident @6.14-6.16 (raw "x4"))
					(e-ident @6.18-6.20 (raw "x6"))
					(e-ident @6.22-6.24 (raw "x8")))))
		(s-decl @10.1-19.2
			(p-ident @10.1-10.7 (raw "result"))
			(e-apply @10.10-19.2
				(e-ident @10.10-10.22 (raw "multi_arg_fn"))
				(e-int @11.5-11.7 (raw "42"))
				(e-string @12.5-12.12
					(e-string-part @12.6-12.11 (raw "hello")))
				(e-string @13.5-13.12
					(e-string-part @13.6-13.11 (raw "world")))
				(e-frac @14.5-14.8 (raw "1.5"))
				(e-frac @15.5-15.9 (raw "3.14"))
				(e-list @16.5-16.11
					(e-int @16.6-16.7 (raw "1"))
					(e-int @16.9-16.10 (raw "2")))
				(e-tag @17.5-17.9 (raw "True"))
				(e-string @18.5-18.11
					(e-string-part @18.6-18.10 (raw "done")))))))
~~~
# FORMATTED
~~~roc
module []

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
		(p-assign @5.1-5.13 (ident "multi_arg_fn"))
		(e-lambda @5.16-6.25
			(args
				(p-assign @5.17-5.19 (ident "x1"))
				(p-assign @5.21-5.23 (ident "x2"))
				(p-assign @5.25-5.27 (ident "x3"))
				(p-assign @5.29-5.31 (ident "x4"))
				(p-assign @5.33-5.35 (ident "x5"))
				(p-assign @5.37-5.39 (ident "x6"))
				(p-assign @5.41-5.43 (ident "x7"))
				(p-assign @5.45-5.47 (ident "x8")))
			(e-tuple @6.5-6.25
				(elems
					(e-lookup-local @6.6-6.8
						(p-assign @5.17-5.19 (ident "x1")))
					(e-lookup-local @6.10-6.12
						(p-assign @5.21-5.23 (ident "x2")))
					(e-lookup-local @6.14-6.16
						(p-assign @5.29-5.31 (ident "x4")))
					(e-lookup-local @6.18-6.20
						(p-assign @5.37-5.39 (ident "x6")))
					(e-lookup-local @6.22-6.24
						(p-assign @5.45-5.47 (ident "x8"))))))
		(annotation @5.1-5.13
			(declared-type
				(ty-fn @4.16-4.57 (effectful false)
					(ty-var @4.16-4.17 (name "a"))
					(ty-var @4.19-4.20 (name "b"))
					(ty-var @4.22-4.23 (name "a"))
					(ty-var @4.25-4.26 (name "c"))
					(ty-var @4.28-4.29 (name "a"))
					(ty-var @4.31-4.32 (name "d"))
					(ty-var @4.34-4.35 (name "a"))
					(ty-var @4.37-4.38 (name "e"))
					(ty-tuple @4.42-4.57
						(ty-var @4.43-4.44 (name "a"))
						(ty-var @4.46-4.47 (name "b"))
						(ty-var @4.49-4.50 (name "c"))
						(ty-var @4.52-4.53 (name "d"))
						(ty-var @4.55-4.56 (name "e")))))))
	(d-let
		(p-assign @10.1-10.7 (ident "result"))
		(e-call @10.10-19.2
			(e-lookup-local @10.10-10.22
				(p-assign @5.1-5.13 (ident "multi_arg_fn")))
			(e-int @11.5-11.7 (value "42"))
			(e-string @12.5-12.12
				(e-literal @12.6-12.11 (string "hello")))
			(e-string @13.5-13.12
				(e-literal @13.6-13.11 (string "world")))
			(e-dec-small @14.5-14.8 (numerator "15") (denominator-power-of-ten "1") (value "1.5"))
			(e-dec-small @15.5-15.9 (numerator "314") (denominator-power-of-ten "2") (value "3.14"))
			(e-list @16.5-16.11
				(elems
					(e-int @16.6-16.7 (value "1"))
					(e-int @16.9-16.10 (value "2"))))
			(e-nominal @17.5-17.9 (nominal "Bool")
				(e-tag @17.5-17.9 (name "True")))
			(e-string @18.5-18.11
				(e-literal @18.6-18.10 (string "done"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.13 (type "a, b, a, c, a, d, a, e -> (a, b, c, d, e)"))
		(patt @10.1-10.7 (type "_f")))
	(expressions
		(expr @5.16-6.25 (type "a, b, a, c, a, d, a, e -> (a, b, c, d, e)"))
		(expr @10.10-19.2 (type "_f"))))
~~~
