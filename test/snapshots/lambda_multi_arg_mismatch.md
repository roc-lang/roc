# META
~~~ini
description=Lambda with multiple non-consecutive argument type mismatches
type=file
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
MISSING MAIN! FUNCTION - lambda_multi_arg_mismatch.md:2:1:17:2
UNUSED VARIABLE - lambda_multi_arg_mismatch.md:3:25:3:27
UNUSED VARIABLE - lambda_multi_arg_mismatch.md:3:33:3:35
UNUSED VARIABLE - lambda_multi_arg_mismatch.md:3:41:3:43
TYPE MISMATCH - lambda_multi_arg_mismatch.md:9:5:9:5
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**lambda_multi_arg_mismatch.md:2:1:17:2:**
```roc
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
```


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
    _Str_

`multi_arg_fn` needs these arguments to have compatible types.

# TOKENS
~~~zig
LowerIdent(2:1-2:13),OpColon(2:14-2:15),LowerIdent(2:16-2:17),Comma(2:17-2:18),LowerIdent(2:19-2:20),Comma(2:20-2:21),LowerIdent(2:22-2:23),Comma(2:23-2:24),LowerIdent(2:25-2:26),Comma(2:26-2:27),LowerIdent(2:28-2:29),Comma(2:29-2:30),LowerIdent(2:31-2:32),Comma(2:32-2:33),LowerIdent(2:34-2:35),Comma(2:35-2:36),LowerIdent(2:37-2:38),OpArrow(2:39-2:41),OpenRound(2:42-2:43),LowerIdent(2:43-2:44),Comma(2:44-2:45),LowerIdent(2:46-2:47),Comma(2:47-2:48),LowerIdent(2:49-2:50),Comma(2:50-2:51),LowerIdent(2:52-2:53),Comma(2:53-2:54),LowerIdent(2:55-2:56),CloseRound(2:56-2:57),
LowerIdent(3:1-3:13),OpAssign(3:14-3:15),OpBar(3:16-3:17),LowerIdent(3:17-3:19),Comma(3:19-3:20),LowerIdent(3:21-3:23),Comma(3:23-3:24),LowerIdent(3:25-3:27),Comma(3:27-3:28),LowerIdent(3:29-3:31),Comma(3:31-3:32),LowerIdent(3:33-3:35),Comma(3:35-3:36),LowerIdent(3:37-3:39),Comma(3:39-3:40),LowerIdent(3:41-3:43),Comma(3:43-3:44),LowerIdent(3:45-3:47),OpBar(3:47-3:48),
OpenRound(4:5-4:6),LowerIdent(4:6-4:8),Comma(4:8-4:9),LowerIdent(4:10-4:12),Comma(4:12-4:13),LowerIdent(4:14-4:16),Comma(4:16-4:17),LowerIdent(4:18-4:20),Comma(4:20-4:21),LowerIdent(4:22-4:24),CloseRound(4:24-4:25),
LowerIdent(8:1-8:7),OpAssign(8:8-8:9),LowerIdent(8:10-8:22),NoSpaceOpenRound(8:22-8:23),
Int(9:5-9:7),Comma(9:7-9:8),
StringStart(10:5-10:6),StringPart(10:6-10:11),StringEnd(10:11-10:12),Comma(10:12-10:13),
StringStart(11:5-11:6),StringPart(11:6-11:11),StringEnd(11:11-11:12),Comma(11:12-11:13),
Float(12:5-12:8),Comma(12:8-12:9),
Float(13:5-13:9),Comma(13:9-13:10),
OpenSquare(14:5-14:6),Int(14:6-14:7),Comma(14:7-14:8),Int(14:9-14:10),CloseSquare(14:10-14:11),Comma(14:11-14:12),
UpperIdent(15:5-15:9),Comma(15:9-15:10),
StringStart(16:5-16:6),StringPart(16:6-16:10),StringEnd(16:10-16:11),Comma(16:11-16:12),
CloseRound(17:1-17:2),
EndOfFile(18:1-18:1),
~~~
# PARSE
~~~clojure
(file @2.1-17.2
	(type-module @2.1-2.13)
	(statements
		(s-type-anno @2.1-2.57 (name "multi_arg_fn")
			(ty-fn @2.16-2.57
				(ty-var @2.16-2.17 (raw "a"))
				(ty-var @2.19-2.20 (raw "b"))
				(ty-var @2.22-2.23 (raw "a"))
				(ty-var @2.25-2.26 (raw "c"))
				(ty-var @2.28-2.29 (raw "a"))
				(ty-var @2.31-2.32 (raw "d"))
				(ty-var @2.34-2.35 (raw "a"))
				(ty-var @2.37-2.38 (raw "e"))
				(ty-tuple @2.42-2.57
					(ty-var @2.43-2.44 (raw "a"))
					(ty-var @2.46-2.47 (raw "b"))
					(ty-var @2.49-2.50 (raw "c"))
					(ty-var @2.52-2.53 (raw "d"))
					(ty-var @2.55-2.56 (raw "e")))))
		(s-decl @3.1-4.25
			(p-ident @3.1-3.13 (raw "multi_arg_fn"))
			(e-lambda @3.16-4.25
				(args
					(p-ident @3.17-3.19 (raw "x1"))
					(p-ident @3.21-3.23 (raw "x2"))
					(p-ident @3.25-3.27 (raw "x3"))
					(p-ident @3.29-3.31 (raw "x4"))
					(p-ident @3.33-3.35 (raw "x5"))
					(p-ident @3.37-3.39 (raw "x6"))
					(p-ident @3.41-3.43 (raw "x7"))
					(p-ident @3.45-3.47 (raw "x8")))
				(e-tuple @4.5-4.25
					(e-ident @4.6-4.8 (raw "x1"))
					(e-ident @4.10-4.12 (raw "x2"))
					(e-ident @4.14-4.16 (raw "x4"))
					(e-ident @4.18-4.20 (raw "x6"))
					(e-ident @4.22-4.24 (raw "x8")))))
		(s-decl @8.1-17.2
			(p-ident @8.1-8.7 (raw "result"))
			(e-apply @8.10-17.2
				(e-ident @8.10-8.22 (raw "multi_arg_fn"))
				(e-int @9.5-9.7 (raw "42"))
				(e-string @10.5-10.12
					(e-string-part @10.6-10.11 (raw "hello")))
				(e-string @11.5-11.12
					(e-string-part @11.6-11.11 (raw "world")))
				(e-frac @12.5-12.8 (raw "1.5"))
				(e-frac @13.5-13.9 (raw "3.14"))
				(e-list @14.5-14.11
					(e-int @14.6-14.7 (raw "1"))
					(e-int @14.9-14.10 (raw "2")))
				(e-tag @15.5-15.9 (raw "True"))
				(e-string @16.5-16.11
					(e-string-part @16.6-16.10 (raw "done")))))))
~~~
# FORMATTED
~~~roc
# Function with 8 arguments where several types must match (a appears in positions 1, 3, 5, 7)
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
		(p-assign @3.1-3.13 (ident "multi_arg_fn"))
		(e-lambda @3.16-4.25
			(args
				(p-assign @3.17-3.19 (ident "x1"))
				(p-assign @3.21-3.23 (ident "x2"))
				(p-assign @3.25-3.27 (ident "x3"))
				(p-assign @3.29-3.31 (ident "x4"))
				(p-assign @3.33-3.35 (ident "x5"))
				(p-assign @3.37-3.39 (ident "x6"))
				(p-assign @3.41-3.43 (ident "x7"))
				(p-assign @3.45-3.47 (ident "x8")))
			(e-tuple @4.5-4.25
				(elems
					(e-lookup-local @4.6-4.8
						(p-assign @3.17-3.19 (ident "x1")))
					(e-lookup-local @4.10-4.12
						(p-assign @3.21-3.23 (ident "x2")))
					(e-lookup-local @4.14-4.16
						(p-assign @3.29-3.31 (ident "x4")))
					(e-lookup-local @4.18-4.20
						(p-assign @3.37-3.39 (ident "x6")))
					(e-lookup-local @4.22-4.24
						(p-assign @3.45-3.47 (ident "x8"))))))
		(annotation @3.1-3.13
			(declared-type
				(ty-fn @2.16-2.57 (effectful false)
					(ty-var @2.16-2.17 (name "a"))
					(ty-var @2.19-2.20 (name "b"))
					(ty-var @2.22-2.23 (name "a"))
					(ty-var @2.25-2.26 (name "c"))
					(ty-var @2.28-2.29 (name "a"))
					(ty-var @2.31-2.32 (name "d"))
					(ty-var @2.34-2.35 (name "a"))
					(ty-var @2.37-2.38 (name "e"))
					(ty-tuple @2.42-2.57
						(ty-var @2.43-2.44 (name "a"))
						(ty-var @2.46-2.47 (name "b"))
						(ty-var @2.49-2.50 (name "c"))
						(ty-var @2.52-2.53 (name "d"))
						(ty-var @2.55-2.56 (name "e")))))))
	(d-let
		(p-assign @8.1-8.7 (ident "result"))
		(e-call @8.10-17.2
			(e-lookup-local @8.10-8.22
				(p-assign @3.1-3.13 (ident "multi_arg_fn")))
			(e-int @9.5-9.7 (value "42"))
			(e-string @10.5-10.12
				(e-literal @10.6-10.11 (string "hello")))
			(e-string @11.5-11.12
				(e-literal @11.6-11.11 (string "world")))
			(e-dec-small @12.5-12.8 (numerator "15") (denominator-power-of-ten "1") (value "1.5"))
			(e-dec-small @13.5-13.9 (numerator "314") (denominator-power-of-ten "2") (value "3.14"))
			(e-list @14.5-14.11
				(elems
					(e-int @14.6-14.7 (value "1"))
					(e-int @14.9-14.10 (value "2"))))
			(e-nominal @15.5-15.9 (nominal "Bool")
				(e-tag @15.5-15.9 (name "True")))
			(e-string @16.5-16.11
				(e-literal @16.6-16.10 (string "done"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.13 (type "a, b, a, c, a, d, a, e -> (a, b, c, d, e)"))
		(patt @8.1-8.7 (type "_f")))
	(expressions
		(expr @3.16-4.25 (type "a, b, a, c, a, d, a, e -> (a, b, c, d, e)"))
		(expr @8.10-17.2 (type "_f"))))
~~~
