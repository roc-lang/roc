# META
~~~ini
description=Polymorphic function instantiation with arity mismatch
type=expr
~~~
# SOURCE
~~~roc
{
    identity : (a, b) -> (a, b)
    identity = |pair| pair

    identity(1, 2)
}
~~~
# EXPECTED
TOO MANY ARGUMENTS - test_instantiation_arity_mismatch.md:5:5:5:19
# PROBLEMS
**TOO MANY ARGUMENTS**
The function `identity` expects 1 argument, but 2 were provided:
**test_instantiation_arity_mismatch.md:5:5:5:19:**
```roc
    identity(1, 2)
```
    ^^^^^^^^^^^^^^

The function has the signature:

    (a, b) -> (a, b)

# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpColon,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpArrow,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-type-anno (name "identity")
			(ty-fn
				(ty-tuple
					(ty-var (raw "a"))
					(ty-var (raw "b")))
				(ty-tuple
					(ty-var (raw "a"))
					(ty-var (raw "b")))))
		(s-decl
			(p-ident (raw "identity"))
			(e-lambda
				(args
					(p-ident (raw "pair")))
				(e-ident (raw "pair"))))
		(e-apply
			(e-ident (raw "identity"))
			(e-int (raw "1"))
			(e-int (raw "2")))))
~~~
# FORMATTED
~~~roc
{
	identity : (a, b) -> (a, b)
	identity = |pair| pair

	identity(1, 2)
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "identity"))
		(e-lambda
			(args
				(p-assign (ident "pair")))
			(e-lookup-local
				(p-assign (ident "pair")))))
	(e-call
		(e-lookup-local
			(p-assign (ident "identity")))
		(e-num (value "1"))
		(e-num (value "2"))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
