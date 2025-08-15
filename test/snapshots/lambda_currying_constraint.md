# META
~~~ini
description=Lambda currying with polymorphic function constraints - tests if numeric literals in curried functions get properly constrained
type=file
~~~
# SOURCE
~~~roc
module [makeAdder, curriedAdd, applyTwice]

# Function that returns a function with polymorphic type
makeAdder : a -> (a -> a)
makeAdder = |x| |y| x + y

# Should constrain the literal 5 to I64
curriedAdd : I64 -> I64
curriedAdd = makeAdder(5)

# Higher-order function that applies a function twice
applyTwice : (a -> a), a -> a
applyTwice = |f, x| f(f(x))

# Should constrain the literal 3 to I64
addThreeTwice : I64 -> I64
addThreeTwice = |n| applyTwice(|x| x + 3, n)
~~~
# EXPECTED
PARSE ERROR - lambda_currying_constraint.md:12:22:12:23
PARSE ERROR - lambda_currying_constraint.md:12:24:12:25
PARSE ERROR - lambda_currying_constraint.md:12:26:12:28
PARSE ERROR - lambda_currying_constraint.md:12:29:12:30
TYPE MISMATCH - lambda_currying_constraint.md:5:21:5:22
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**lambda_currying_constraint.md:12:22:12:23:**
```roc
applyTwice : (a -> a), a -> a
```
                     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**lambda_currying_constraint.md:12:24:12:25:**
```roc
applyTwice : (a -> a), a -> a
```
                       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**lambda_currying_constraint.md:12:26:12:28:**
```roc
applyTwice : (a -> a), a -> a
```
                         ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**lambda_currying_constraint.md:12:29:12:30:**
```roc
applyTwice : (a -> a), a -> a
```
                            ^


**TYPE MISMATCH**
This expression is used in an unexpected way:
**lambda_currying_constraint.md:5:21:5:22:**
```roc
makeAdder = |x| |y| x + y
```
                    ^

It is of type:
    _a_

But you are trying to use it as:
    _Num(_size)_

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:18),Comma(1:18-1:19),LowerIdent(1:20-1:30),Comma(1:30-1:31),LowerIdent(1:32-1:42),CloseSquare(1:42-1:43),
LowerIdent(4:1-4:10),OpColon(4:11-4:12),LowerIdent(4:13-4:14),OpArrow(4:15-4:17),OpenRound(4:18-4:19),LowerIdent(4:19-4:20),OpArrow(4:21-4:23),LowerIdent(4:24-4:25),CloseRound(4:25-4:26),
LowerIdent(5:1-5:10),OpAssign(5:11-5:12),OpBar(5:13-5:14),LowerIdent(5:14-5:15),OpBar(5:15-5:16),OpBar(5:17-5:18),LowerIdent(5:18-5:19),OpBar(5:19-5:20),LowerIdent(5:21-5:22),OpPlus(5:23-5:24),LowerIdent(5:25-5:26),
LowerIdent(8:1-8:11),OpColon(8:12-8:13),UpperIdent(8:14-8:17),OpArrow(8:18-8:20),UpperIdent(8:21-8:24),
LowerIdent(9:1-9:11),OpAssign(9:12-9:13),LowerIdent(9:14-9:23),NoSpaceOpenRound(9:23-9:24),Int(9:24-9:25),CloseRound(9:25-9:26),
LowerIdent(12:1-12:11),OpColon(12:12-12:13),OpenRound(12:14-12:15),LowerIdent(12:15-12:16),OpArrow(12:17-12:19),LowerIdent(12:20-12:21),CloseRound(12:21-12:22),Comma(12:22-12:23),LowerIdent(12:24-12:25),OpArrow(12:26-12:28),LowerIdent(12:29-12:30),
LowerIdent(13:1-13:11),OpAssign(13:12-13:13),OpBar(13:14-13:15),LowerIdent(13:15-13:16),Comma(13:16-13:17),LowerIdent(13:18-13:19),OpBar(13:19-13:20),LowerIdent(13:21-13:22),NoSpaceOpenRound(13:22-13:23),LowerIdent(13:23-13:24),NoSpaceOpenRound(13:24-13:25),LowerIdent(13:25-13:26),CloseRound(13:26-13:27),CloseRound(13:27-13:28),
LowerIdent(16:1-16:14),OpColon(16:15-16:16),UpperIdent(16:17-16:20),OpArrow(16:21-16:23),UpperIdent(16:24-16:27),
LowerIdent(17:1-17:14),OpAssign(17:15-17:16),OpBar(17:17-17:18),LowerIdent(17:18-17:19),OpBar(17:19-17:20),LowerIdent(17:21-17:31),NoSpaceOpenRound(17:31-17:32),OpBar(17:32-17:33),LowerIdent(17:33-17:34),OpBar(17:34-17:35),LowerIdent(17:36-17:37),OpPlus(17:38-17:39),Int(17:40-17:41),Comma(17:41-17:42),LowerIdent(17:43-17:44),CloseRound(17:44-17:45),EndOfFile(17:45-17:45),
~~~
# PARSE
~~~clojure
(file @1.1-17.45
	(module @1.1-1.43
		(exposes @1.8-1.43
			(exposed-lower-ident @1.9-1.18
				(text "makeAdder"))
			(exposed-lower-ident @1.20-1.30
				(text "curriedAdd"))
			(exposed-lower-ident @1.32-1.42
				(text "applyTwice"))))
	(statements
		(s-type-anno @4.1-4.26 (name "makeAdder")
			(ty-fn @4.13-4.26
				(ty-var @4.13-4.14 (raw "a"))
				(ty-fn @4.19-4.25
					(ty-var @4.19-4.20 (raw "a"))
					(ty-var @4.24-4.25 (raw "a")))))
		(s-decl @5.1-5.26
			(p-ident @5.1-5.10 (raw "makeAdder"))
			(e-lambda @5.13-5.26
				(args
					(p-ident @5.14-5.15 (raw "x")))
				(e-lambda @5.17-5.26
					(args
						(p-ident @5.18-5.19 (raw "y")))
					(e-binop @5.21-5.26 (op "+")
						(e-ident @5.21-5.22 (raw "x"))
						(e-ident @5.25-5.26 (raw "y"))))))
		(s-type-anno @8.1-8.24 (name "curriedAdd")
			(ty-fn @8.14-8.24
				(ty @8.14-8.17 (name "I64"))
				(ty @8.21-8.24 (name "I64"))))
		(s-decl @9.1-9.26
			(p-ident @9.1-9.11 (raw "curriedAdd"))
			(e-apply @9.14-9.26
				(e-ident @9.14-9.23 (raw "makeAdder"))
				(e-int @9.24-9.25 (raw "5"))))
		(s-type-anno @12.1-12.22 (name "applyTwice")
			(ty-fn @12.15-12.21
				(ty-var @12.15-12.16 (raw "a"))
				(ty-var @12.20-12.21 (raw "a"))))
		(s-malformed @12.22-12.23 (tag "statement_unexpected_token"))
		(s-malformed @12.24-12.25 (tag "statement_unexpected_token"))
		(s-malformed @12.26-12.28 (tag "statement_unexpected_token"))
		(s-malformed @12.29-12.30 (tag "statement_unexpected_token"))
		(s-decl @13.1-13.28
			(p-ident @13.1-13.11 (raw "applyTwice"))
			(e-lambda @13.14-13.28
				(args
					(p-ident @13.15-13.16 (raw "f"))
					(p-ident @13.18-13.19 (raw "x")))
				(e-apply @13.21-13.28
					(e-ident @13.21-13.22 (raw "f"))
					(e-apply @13.23-13.27
						(e-ident @13.23-13.24 (raw "f"))
						(e-ident @13.25-13.26 (raw "x"))))))
		(s-type-anno @16.1-16.27 (name "addThreeTwice")
			(ty-fn @16.17-16.27
				(ty @16.17-16.20 (name "I64"))
				(ty @16.24-16.27 (name "I64"))))
		(s-decl @17.1-17.45
			(p-ident @17.1-17.14 (raw "addThreeTwice"))
			(e-lambda @17.17-17.45
				(args
					(p-ident @17.18-17.19 (raw "n")))
				(e-apply @17.21-17.45
					(e-ident @17.21-17.31 (raw "applyTwice"))
					(e-lambda @17.32-17.41
						(args
							(p-ident @17.33-17.34 (raw "x")))
						(e-binop @17.36-17.41 (op "+")
							(e-ident @17.36-17.37 (raw "x"))
							(e-int @17.40-17.41 (raw "3"))))
					(e-ident @17.43-17.44 (raw "n")))))))
~~~
# FORMATTED
~~~roc
module [makeAdder, curriedAdd, applyTwice]

# Function that returns a function with polymorphic type
makeAdder : a -> (a -> a)
makeAdder = |x| |y| x + y

# Should constrain the literal 5 to I64
curriedAdd : I64 -> I64
curriedAdd = makeAdder(5)

# Higher-order function that applies a function twice
applyTwice : (a -> a)

applyTwice = |f, x| f(f(x))

# Should constrain the literal 3 to I64
addThreeTwice : I64 -> I64
addThreeTwice = |n| applyTwice(|x| x + 3, n)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.10 (ident "makeAdder"))
		(e-lambda @5.13-5.26
			(args
				(p-assign @5.14-5.15 (ident "x")))
			(e-closure @5.17-5.26
				(captures
					(capture @5.14-5.15 (ident "x")))
				(e-lambda @5.17-5.26
					(args
						(p-assign @5.18-5.19 (ident "y")))
					(e-binop @5.21-5.26 (op "add")
						(e-lookup-local @5.21-5.22
							(p-assign @5.14-5.15 (ident "x")))
						(e-lookup-local @5.25-5.26
							(p-assign @5.18-5.19 (ident "y")))))))
		(annotation @5.1-5.10
			(declared-type
				(ty-fn @4.13-4.26 (effectful false)
					(ty-var @4.13-4.14 (name "a"))
					(ty-parens @4.18-4.26
						(ty-fn @4.19-4.25 (effectful false)
							(ty-var @4.19-4.20 (name "a"))
							(ty-var @4.24-4.25 (name "a"))))))))
	(d-let
		(p-assign @9.1-9.11 (ident "curriedAdd"))
		(e-call @9.14-9.26
			(e-lookup-local @9.14-9.23
				(p-assign @5.1-5.10 (ident "makeAdder")))
			(e-int @9.24-9.25 (value "5")))
		(annotation @9.1-9.11
			(declared-type
				(ty-fn @8.14-8.24 (effectful false)
					(ty @8.14-8.17 (name "I64"))
					(ty @8.21-8.24 (name "I64"))))))
	(d-let
		(p-assign @13.1-13.11 (ident "applyTwice"))
		(e-lambda @13.14-13.28
			(args
				(p-assign @13.15-13.16 (ident "f"))
				(p-assign @13.18-13.19 (ident "x")))
			(e-call @13.21-13.28
				(e-lookup-local @13.21-13.22
					(p-assign @13.15-13.16 (ident "f")))
				(e-call @13.23-13.27
					(e-lookup-local @13.23-13.24
						(p-assign @13.15-13.16 (ident "f")))
					(e-lookup-local @13.25-13.26
						(p-assign @13.18-13.19 (ident "x")))))))
	(d-let
		(p-assign @17.1-17.14 (ident "addThreeTwice"))
		(e-closure @17.17-17.45
			(captures
				(capture @13.1-13.11 (ident "applyTwice")))
			(e-lambda @17.17-17.45
				(args
					(p-assign @17.18-17.19 (ident "n")))
				(e-call @17.21-17.45
					(e-lookup-local @17.21-17.31
						(p-assign @13.1-13.11 (ident "applyTwice")))
					(e-lambda @17.32-17.41
						(args
							(p-assign @17.33-17.34 (ident "x")))
						(e-binop @17.36-17.41 (op "add")
							(e-lookup-local @17.36-17.37
								(p-assign @17.33-17.34 (ident "x")))
							(e-int @17.40-17.41 (value "3"))))
					(e-lookup-local @17.43-17.44
						(p-assign @17.18-17.19 (ident "n"))))))
		(annotation @17.1-17.14
			(declared-type
				(ty-fn @16.17-16.27 (effectful false)
					(ty @16.17-16.20 (name "I64"))
					(ty @16.24-16.27 (name "I64")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.10 (type "Error -> Error -> Error"))
		(patt @9.1-9.11 (type "Error -> Error"))
		(patt @13.1-13.11 (type "_arg -> ret, _arg2 -> ret2"))
		(patt @17.1-17.14 (type "Error -> Error")))
	(expressions
		(expr @5.13-5.26 (type "Error -> Error -> Error"))
		(expr @9.14-9.26 (type "Error -> Error"))
		(expr @13.14-13.28 (type "_arg -> ret, _arg2 -> ret2"))
		(expr @17.17-17.45 (type "Error -> Error"))))
~~~
