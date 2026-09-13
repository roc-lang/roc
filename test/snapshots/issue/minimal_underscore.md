# META
~~~ini
description=Minimal test - underscore type should become error type
type=snippet
~~~
# SOURCE
~~~roc
BadType := _
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - minimal_underscore.md:1:1:1:1
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Underscore In Type Alias")
		(region (start 1 1) (end 1 1))
		(headline
			(reflow "Underscores are not allowed in type alias declarations."))
		(document
			(source-region (file "minimal_underscore.md") (start 1 1) (end 1 1) (annotation error) (line-text "BadType := _"))
			(line-break)
			(reflow "Underscores in type annotations mean \"I don't care about this type\", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead."))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,Underscore,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "BadType")
				(args))
			(_))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl
		(ty-header (name "BadType"))
		(ty-underscore)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal (type "BadType")
			(ty-header (name "BadType"))))
	(expressions))
~~~
