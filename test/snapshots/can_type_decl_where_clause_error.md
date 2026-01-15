# META
~~~ini
description=Test where clause not allowed in type declaration
type=expr
~~~
# SOURCE
~~~roc
{
    Counter := U64 where [a.foo : Str]
    x = 5
    x
}
~~~
# EXPECTED
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - can_type_decl_where_clause_error.md:2:5:2:39
# PROBLEMS
**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**can_type_decl_where_clause_error.md:2:5:2:39:**
```roc
    Counter := U64 where [a.foo : Str]
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
OpenCurly,
UpperIdent,OpColonEqual,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,UpperIdent,CloseSquare,
LowerIdent,OpAssign,Int,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-type-decl
			(header (name "Counter")
				(args))
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "5")))
		(e-ident (raw "x"))))
~~~
# FORMATTED
~~~roc
{
	Counter := U64 where [a.foo : Str]
	x = 5
	x
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-nominal-decl
		(ty-header (name "Counter"))
		(ty-lookup (name "U64") (builtin)))
	(s-let
		(p-assign (ident "x"))
		(e-num (value "5")))
	(e-lookup-local
		(p-assign (ident "x"))))
~~~
# TYPES
~~~clojure
(expr (type "b where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
~~~
