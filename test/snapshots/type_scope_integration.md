# META
~~~ini
description=Type declaration scope integration - redeclaration and undeclared type errors
type=snippet
~~~
# SOURCE
~~~roc
# First declare a type
Foo : U64

# Try to redeclare the same type (should error)
Foo : Str

# Declare another type that uses an undeclared type
Bar : SomeUndeclaredType

# Declare a type that properly uses a declared type
Baz : Foo
~~~
# EXPECTED
TYPE REDECLARED - type_scope_integration.md:5:1:5:10
UNDECLARED TYPE - type_scope_integration.md:8:7:8:25
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 5 1) (end 5 10))
		(headline
			(reflow "The type ")
			(annotated code "Foo")
			(reflow " is being redeclared."))
		(document
			(source-region (file "type_scope_integration.md") (start 5 1) (end 5 10) (annotation error) (line-text "Foo : Str"))
			(line-break)
			(reflow "But ")
			(annotated type "Foo")
			(reflow " was already declared in ")
			(source-location
				(file "type_scope_integration.md")
				(line 2)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "type_scope_integration.md") (start 2 1) (end 2 10) (annotation dim) (line-text "Foo : U64"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 8 7) (end 8 25))
		(headline
			(reflow "The type ")
			(annotated code "SomeUndeclaredType")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "type_scope_integration.md") (start 8 7) (end 8 25) (annotation error) (line-text "Bar : SomeUndeclaredType")))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty (name "U64")))
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty (name "Str")))
		(s-type-decl
			(header (name "Bar")
				(args))
			(ty (name "SomeUndeclaredType")))
		(s-type-decl
			(header (name "Baz")
				(args))
			(ty (name "Foo")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "Foo"))
		(ty-lookup (name "U64") (builtin)))
	(s-alias-decl
		(ty-header (name "Foo"))
		(ty-lookup (name "Str") (builtin)))
	(s-alias-decl
		(ty-header (name "Bar"))
		(ty-malformed))
	(s-alias-decl
		(ty-header (name "Baz"))
		(ty-lookup (name "Foo") (local))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "Foo")
			(ty-header (name "Foo")))
		(alias (type "Foo")
			(ty-header (name "Foo")))
		(alias (type "Error")
			(ty-header (name "Bar")))
		(alias (type "Baz")
			(ty-header (name "Baz"))))
	(expressions))
~~~
