# META
~~~ini
description=Static dispatch type mismatch - calling .repeat on List should error since List has no repeat method
type=snippet
~~~
# SOURCE
~~~roc
# Calling .repeat(3) on a List should fail to compile, since List has no repeat method.
# Only Str has a repeat method.
lst = [1, 2, 3]
result = lst.repeat(3)
~~~
# EXPECTED
MISSING METHOD - static_dispatch_type_mismatch.md:4:14:4:20
# PROBLEMS
**MISSING METHOD**
This **repeat** method is being called on a value whose type doesn't have that method:
**static_dispatch_type_mismatch.md:4:14:4:20:**
```roc
result = lst.repeat(3)
```
             ^^^^^^

The value's type, which does not have a method named **repeat**, is:

    List(b) where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]

**Hint:** For this to work, the type would need to have a method named **repeat** associated with it in the type's declaration.

# TOKENS
~~~zig
LowerIdent,OpAssign,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "lst"))
			(e-list
				(e-int (raw "1"))
				(e-int (raw "2"))
				(e-int (raw "3"))))
		(s-decl
			(p-ident (raw "result"))
			(e-field-access
				(e-ident (raw "lst"))
				(e-apply
					(e-ident (raw "repeat"))
					(e-int (raw "3")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "lst"))
		(e-list
			(elems
				(e-num (value "1"))
				(e-num (value "2"))
				(e-num (value "3")))))
	(d-let
		(p-assign (ident "result"))
		(e-dot-access (field "repeat")
			(receiver
				(e-lookup-local
					(p-assign (ident "lst"))))
			(args
				(e-num (value "3"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(b) where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "Error")))
	(expressions
		(expr (type "List(b) where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "Error"))))
~~~
