# META
~~~ini
description=tuple_type
type=expr
~~~
# SOURCE
~~~roc
{
    f : (Str, Str) -> (Str, Str)
    f = |x| x

    f((1, 2))
}
~~~
# EXPECTED
TYPE DOES NOT HAVE METHODS - tuple_type.md:5:8:5:9
TYPE DOES NOT HAVE METHODS - tuple_type.md:5:11:5:12
# PROBLEMS
**TYPE DOES NOT HAVE METHODS**
You're calling the method `from_int_digits` on a type that doesn't support methods:
**tuple_type.md:5:8:5:9:**
```roc
    f((1, 2))
```
       ^

This type doesn't support methods:
    _Str_



**TYPE DOES NOT HAVE METHODS**
You're calling the method `from_int_digits` on a type that doesn't support methods:
**tuple_type.md:5:11:5:12:**
```roc
    f((1, 2))
```
          ^

This type doesn't support methods:
    _Str_



# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpColon,OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,OpArrow,OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,NoSpaceOpenRound,NoSpaceOpenRound,Int,Comma,Int,CloseRound,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-type-anno (name "f")
			(ty-fn
				(ty-tuple
					(ty (name "Str"))
					(ty (name "Str")))
				(ty-tuple
					(ty (name "Str"))
					(ty (name "Str")))))
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x"))))
		(e-apply
			(e-ident (raw "f"))
			(e-tuple
				(e-int (raw "1"))
				(e-int (raw "2"))))))
~~~
# FORMATTED
~~~roc
{
	f : (Str, Str) -> (Str, Str)
	f = |x| x

	f((1, 2))
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x")))))
	(e-call
		(e-lookup-local
			(p-assign (ident "f")))
		(e-tuple
			(elems
				(e-num (value "1"))
				(e-num (value "2"))))))
~~~
# TYPES
~~~clojure
(expr (type "(Str, Str)"))
~~~
