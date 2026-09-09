# META
~~~ini
description=Undeclared type usage should produce error
type=snippet
~~~
# SOURCE
~~~roc
MyType : UnknownType

processValue : UndeclaredResult -> Str
processValue = |value| {
    "processed"
}

AnotherType : SomeMod.MissingType
~~~
# EXPECTED
UNDECLARED TYPE - type_undeclared_usage.md:1:10:1:21
UNDECLARED TYPE - type_undeclared_usage.md:3:16:3:32
UNUSED VARIABLE - type_undeclared_usage.md:4:17:4:22
MOD NOT IMPORTED - type_undeclared_usage.md:8:15:8:34
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 1 10) (end 1 21))
		(headline
			(reflow "The type ")
			(annotated code "UnknownType")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "type_undeclared_usage.md") (start 1 10) (end 1 21) (annotation error) (line-text "MyType : UnknownType"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 3 16) (end 3 32))
		(headline
			(reflow "The type ")
			(annotated code "UndeclaredResult")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "type_undeclared_usage.md") (start 3 16) (end 3 32) (annotation error) (line-text "processValue : UndeclaredResult -> Str"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 4 17) (end 4 22))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "value")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_value")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "type_undeclared_usage.md") (start 4 17) (end 4 22) (annotation error) (line-text "processValue = |value| {"))))
	(report
		(severity runtime_error)
		(title "Mod Not Imported")
		(region (start 8 15) (end 8 34))
		(headline
			(text "There is no mod with the name ")
			(annotated code "SomeMod")
			(reflow " imported into this Roc file."))
		(document
			(source-region (file "type_undeclared_usage.md") (start 8 15) (end 8 34) (annotation error) (line-text "AnotherType : SomeMod.MissingType")))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColon,UpperIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
StringStart,StringPart,StringEnd,
CloseCurly,
UpperIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
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
			(ty (name "UnknownType")))
		(s-type-anno (name "processValue")
			(ty-fn
				(ty (name "UndeclaredResult"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "processValue"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-block
					(statements
						(e-string
							(e-string-part (raw "processed")))))))
		(s-type-decl
			(header (name "AnotherType")
				(args))
			(ty (name "SomeMod.MissingType")))))
~~~
# FORMATTED
~~~roc
MyType : UnknownType

processValue : UndeclaredResult -> Str
processValue = |value| {
	"processed"
}

AnotherType : SomeMod.MissingType
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "processValue"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (builtin)))))
	(s-alias-decl
		(ty-header (name "MyType"))
		(ty-malformed))
	(s-alias-decl
		(ty-header (name "AnotherType"))
		(ty-malformed)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> Str")))
	(type_decls
		(alias (type "Error")
			(ty-header (name "MyType")))
		(alias (type "Error")
			(ty-header (name "AnotherType"))))
	(expressions
		(expr (type "Error -> Str"))))
~~~
