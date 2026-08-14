# META
~~~ini
description=A where alias constraint written against a parameter instead of the receiver is rejected
type=snippet
~~~
# SOURCE
~~~roc
a.EncodableTo(fmt) : where [
	a.encode : a, fmt -> fmt,
	fmt.finish : fmt -> Str,
]
~~~
# EXPECTED
WHERE ALIAS CONSTRAINS ANOTHER TYPE - where_alias_parameter_constraint.md:3:2:3:25
# PROBLEMS
── ✗ where alias constrains another type ─ where_alias_parameter_constraint.md:3:2

A where alias constrains only its receiver, but this constraint is on a
different type variable.

fmt.finish : fmt -> Str,
^^^^^^^^^^^^^^^^^^^^^^^

Write this constraint against a, or declare a separate where alias for the
other type variable and apply it alongside this one.

# TOKENS
~~~zig
LowerIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,KwWhere,OpenSquare,
LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpArrow,LowerIdent,Comma,
LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,Comma,
CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name ".EncodableTo")
				(args
					(ty-var (raw "fmt"))))
			(ty-var (raw "a"))
			(where
				(method (mod-of "a") (name "encode")
					(args
						(ty-var (raw "a"))
						(ty-var (raw "fmt")))
					(ty-var (raw "fmt")))
				(method (mod-of "fmt") (name "finish")
					(args
						(ty-var (raw "fmt")))
					(ty (name "Str")))))))
~~~
# FORMATTED
~~~roc
a.EncodableTo(fmt) :
	where [
		a.encode : a, fmt -> fmt,
		fmt.finish : fmt -> Str,
	]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-where-alias-decl
		(ty-header (name "EncodableTo")
			(ty-args
				(ty-rigid-var (name "fmt"))))
		(ty-rigid-var (name "a"))
		(where
			(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "encode")
				(args
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
					(ty-rigid-var-lookup (ty-rigid-var (name "fmt"))))
				(ty-rigid-var-lookup (ty-rigid-var (name "fmt"))))
			(method (ty-rigid-var-lookup (ty-rigid-var (name "fmt"))) (name "finish")
				(args
					(ty-rigid-var-lookup (ty-rigid-var (name "fmt"))))
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(where-alias (type "a where [a.encode : a, fmt -> fmt, fmt.finish : fmt -> Str]")
			(ty-header (name "EncodableTo")
				(ty-args
					(ty-rigid-var (name "fmt"))))))
	(expressions))
~~~
