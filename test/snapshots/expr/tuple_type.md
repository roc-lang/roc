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
MISSING METHOD - tuple_type.md:5:8:5:9
MISSING METHOD - tuple_type.md:5:11:5:12
# PROBLEMS
**MISSING METHOD**
This **from_numeral** method is being called on the type **Str**, which has no method with that name:
**tuple_type.md:5:8:5:9:**
```roc
    f((1, 2))
```
       ^


**Hint: **For this to work, the type would need to have a method named **from_numeral** associated with it in the type's declaration.

**MISSING METHOD**
This **from_numeral** method is being called on the type **Str**, which has no method with that name:
**tuple_type.md:5:11:5:12:**
```roc
    f((1, 2))
```
          ^


**Hint: **For this to work, the type would need to have a method named **from_numeral** associated with it in the type's declaration.

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
