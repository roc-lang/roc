# META
~~~ini
description=Comprehensive tuple expression tests
type=expr
~~~
# SOURCE
~~~roc
{
    # define these to avoid runtime errors
    add_one = |_| {}
    x = 10
    y = 20
    z = 30

    # example tuples
	empty = ()
	single = (42)
	pair = (1, 2)
	triple = (1, "hello", True)
	nested = ((1, 2), (3, 4))
	mixed = (add_one(5), "world", [1, 2, 3])
	with_vars = (x, y, z)
	with_lambda = (|n| n + 1, 42)

	empty
}
~~~
# EXPECTED
EMPTY TUPLE NOT ALLOWED - tuple_comprehensive.md:9:10:9:12
UNUSED VARIABLE - tuple_comprehensive.md:10:2:10:8
UNUSED VARIABLE - tuple_comprehensive.md:11:2:11:6
UNUSED VARIABLE - tuple_comprehensive.md:12:2:12:8
UNUSED VARIABLE - tuple_comprehensive.md:13:2:13:8
UNUSED VARIABLE - tuple_comprehensive.md:14:2:14:7
UNUSED VARIABLE - tuple_comprehensive.md:15:2:15:11
UNUSED VARIABLE - tuple_comprehensive.md:16:2:16:13
# PROBLEMS
**EMPTY TUPLE NOT ALLOWED**
I am part way through parsing this tuple, but it is empty:
**tuple_comprehensive.md:9:10:9:12:**
```roc
	empty = ()
```
	        ^^

If you want to represent nothing, try using an empty record: `{}`.

**UNUSED VARIABLE**
Variable `single` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_single` to suppress this warning.
The unused variable is declared here:
**tuple_comprehensive.md:10:2:10:8:**
```roc
	single = (42)
```
	^^^^^^


**UNUSED VARIABLE**
Variable `pair` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_pair` to suppress this warning.
The unused variable is declared here:
**tuple_comprehensive.md:11:2:11:6:**
```roc
	pair = (1, 2)
```
	^^^^


**UNUSED VARIABLE**
Variable `triple` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_triple` to suppress this warning.
The unused variable is declared here:
**tuple_comprehensive.md:12:2:12:8:**
```roc
	triple = (1, "hello", True)
```
	^^^^^^


**UNUSED VARIABLE**
Variable `nested` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_nested` to suppress this warning.
The unused variable is declared here:
**tuple_comprehensive.md:13:2:13:8:**
```roc
	nested = ((1, 2), (3, 4))
```
	^^^^^^


**UNUSED VARIABLE**
Variable `mixed` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_mixed` to suppress this warning.
The unused variable is declared here:
**tuple_comprehensive.md:14:2:14:7:**
```roc
	mixed = (add_one(5), "world", [1, 2, 3])
```
	^^^^^


**UNUSED VARIABLE**
Variable `with_vars` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_with_vars` to suppress this warning.
The unused variable is declared here:
**tuple_comprehensive.md:15:2:15:11:**
```roc
	with_vars = (x, y, z)
```
	^^^^^^^^^


**UNUSED VARIABLE**
Variable `with_lambda` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_with_lambda` to suppress this warning.
The unused variable is declared here:
**tuple_comprehensive.md:16:2:16:13:**
```roc
	with_lambda = (|n| n + 1, 42)
```
	^^^^^^^^^^^


# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,CloseCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpenRound,CloseRound,
LowerIdent,OpAssign,OpenRound,Int,CloseRound,
LowerIdent,OpAssign,OpenRound,Int,Comma,Int,CloseRound,
LowerIdent,OpAssign,OpenRound,Int,Comma,StringStart,StringPart,StringEnd,Comma,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpenRound,NoSpaceOpenRound,Int,Comma,Int,CloseRound,Comma,OpenRound,Int,Comma,Int,CloseRound,CloseRound,
LowerIdent,OpAssign,OpenRound,LowerIdent,NoSpaceOpenRound,Int,CloseRound,Comma,StringStart,StringPart,StringEnd,Comma,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpenRound,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,Comma,Int,CloseRound,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "add_one"))
			(e-lambda
				(args
					(p-underscore))
				(e-record)))
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "10")))
		(s-decl
			(p-ident (raw "y"))
			(e-int (raw "20")))
		(s-decl
			(p-ident (raw "z"))
			(e-int (raw "30")))
		(s-decl
			(p-ident (raw "empty"))
			(e-tuple))
		(s-decl
			(p-ident (raw "single"))
			(e-tuple
				(e-int (raw "42"))))
		(s-decl
			(p-ident (raw "pair"))
			(e-tuple
				(e-int (raw "1"))
				(e-int (raw "2"))))
		(s-decl
			(p-ident (raw "triple"))
			(e-tuple
				(e-int (raw "1"))
				(e-string
					(e-string-part (raw "hello")))
				(e-tag (raw "True"))))
		(s-decl
			(p-ident (raw "nested"))
			(e-tuple
				(e-tuple
					(e-int (raw "1"))
					(e-int (raw "2")))
				(e-tuple
					(e-int (raw "3"))
					(e-int (raw "4")))))
		(s-decl
			(p-ident (raw "mixed"))
			(e-tuple
				(e-apply
					(e-ident (raw "add_one"))
					(e-int (raw "5")))
				(e-string
					(e-string-part (raw "world")))
				(e-list
					(e-int (raw "1"))
					(e-int (raw "2"))
					(e-int (raw "3")))))
		(s-decl
			(p-ident (raw "with_vars"))
			(e-tuple
				(e-ident (raw "x"))
				(e-ident (raw "y"))
				(e-ident (raw "z"))))
		(s-decl
			(p-ident (raw "with_lambda"))
			(e-tuple
				(e-lambda
					(args
						(p-ident (raw "n")))
					(e-binop (op "+")
						(e-ident (raw "n"))
						(e-int (raw "1"))))
				(e-int (raw "42"))))
		(e-ident (raw "empty"))))
~~~
# FORMATTED
~~~roc
{
	# define these to avoid runtime errors
	add_one = |_| {}
	x = 10
	y = 20
	z = 30

	# example tuples
	empty = ()
	single = (42)
	pair = (1, 2)
	triple = (1, "hello", True)
	nested = ((1, 2), (3, 4))
	mixed = (add_one(5), "world", [1, 2, 3])
	with_vars = (x, y, z)
	with_lambda = (|n| n + 1, 42)

	empty
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "add_one"))
		(e-lambda
			(args
				(p-underscore))
			(e-empty_record)))
	(s-let
		(p-assign (ident "x"))
		(e-num (value "10")))
	(s-let
		(p-assign (ident "y"))
		(e-num (value "20")))
	(s-let
		(p-assign (ident "z"))
		(e-num (value "30")))
	(s-let
		(p-assign (ident "empty"))
		(e-runtime-error (tag "empty_tuple")))
	(s-let
		(p-assign (ident "single"))
		(e-num (value "42")))
	(s-let
		(p-assign (ident "pair"))
		(e-tuple
			(elems
				(e-num (value "1"))
				(e-num (value "2")))))
	(s-let
		(p-assign (ident "triple"))
		(e-tuple
			(elems
				(e-num (value "1"))
				(e-string
					(e-literal (string "hello")))
				(e-tag (name "True")))))
	(s-let
		(p-assign (ident "nested"))
		(e-tuple
			(elems
				(e-tuple
					(elems
						(e-num (value "1"))
						(e-num (value "2"))))
				(e-tuple
					(elems
						(e-num (value "3"))
						(e-num (value "4")))))))
	(s-let
		(p-assign (ident "mixed"))
		(e-tuple
			(elems
				(e-call
					(e-lookup-local
						(p-assign (ident "add_one")))
					(e-num (value "5")))
				(e-string
					(e-literal (string "world")))
				(e-list
					(elems
						(e-num (value "1"))
						(e-num (value "2"))
						(e-num (value "3")))))))
	(s-let
		(p-assign (ident "with_vars"))
		(e-tuple
			(elems
				(e-lookup-local
					(p-assign (ident "x")))
				(e-lookup-local
					(p-assign (ident "y")))
				(e-lookup-local
					(p-assign (ident "z"))))))
	(s-let
		(p-assign (ident "with_lambda"))
		(e-tuple
			(elems
				(e-lambda
					(args
						(p-assign (ident "n")))
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "n")))
						(e-num (value "1"))))
				(e-num (value "42")))))
	(e-lookup-local
		(p-assign (ident "empty"))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
