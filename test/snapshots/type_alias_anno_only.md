# META
~~~ini
description=Type alias used in annotation-only declaration should resolve correctly
type=snippet
~~~
# SOURCE
~~~roc
MyType : Str

hey : MyType
~~~
# EXPECTED
DECLARATION HAS NO VALUE - type_alias_anno_only.md:3:1:3:13
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 3 1) (end 3 13))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "type_alias_anno_only.md") (start 3 1) (end 3 13) (annotation error) (line-text "hey : MyType"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColon,UpperIdent,
LowerIdent,OpColon,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "MyType")
				(args))
			(ty (name "Str")))
		(s-type-anno (name "hey")
			(ty (name "MyType")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "hey"))
		(e-anno-only)
		(annotation
			(ty-lookup (name "MyType") (local))))
	(s-alias-decl
		(ty-header (name "MyType"))
		(ty-lookup (name "Str") (builtin))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "MyType")))
	(type_decls
		(alias (type "MyType")
			(ty-header (name "MyType"))))
	(expressions
		(expr (type "MyType"))))
~~~
