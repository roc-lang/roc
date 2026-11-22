# META
~~~ini
description=Repro for https://github.com/roc-lang/roc/issues/13285. A lookup of an associated value must not use the associated block of a redeclared type. The selected X is the alias, which has neither one nor f, so both lookups are reported as missing and the duplicate X is reported as redeclared.
type=snippet
~~~
# SOURCE
~~~roc
X : U64

X := [].{
	one = 1
	f = |a| a
}

y : U64
y = X.one

z = X.f(1) + 0
~~~
# EXPECTED
TYPE REDECLARED - type_redeclared_alias_then_nominal_assoc_lookup.md:3:1:6:2
ASSOCIATED CONSTANT NOT FOUND - type_redeclared_alias_then_nominal_assoc_lookup.md:9:5:9:10
ASSOCIATED FUNCTION NOT FOUND - type_redeclared_alias_then_nominal_assoc_lookup.md:11:5:11:8
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 3 1) (end 6 2))
		(headline
			(reflow "The type ")
			(annotated code "X")
			(reflow " is being redeclared."))
		(document
			(source-region (file "type_redeclared_alias_then_nominal_assoc_lookup.md") (start 3 1) (end 6 2) (annotation error) (line-text "X := [].{\n\tone = 1\n\tf = |a| a\n}"))
			(line-break)
			(reflow "But ")
			(annotated type "X")
			(reflow " was already declared in ")
			(source-location
				(file "type_redeclared_alias_then_nominal_assoc_lookup.md")
				(line 1)
				(column 1))
			(reflow ".")
			(line-break)
			(source-region (file "type_redeclared_alias_then_nominal_assoc_lookup.md") (start 1 1) (end 1 8) (annotation dim) (line-text "X : U64"))))
	(report
		(severity runtime_error)
		(title "Associated Constant Not Found")
		(region (start 9 5) (end 9 10))
		(headline
			(reflow "The type")
			(reflow " ")
			(annotated code "X")
			(reflow " ")
			(reflow "does not have an associated constant named")
			(reflow " ")
			(annotated code "one")
			(reflow "."))
		(document
			(source-region (file "type_redeclared_alias_then_nominal_assoc_lookup.md") (start 9 5) (end 9 10) (annotation error) (line-text "y = X.one"))))
	(report
		(severity runtime_error)
		(title "Associated Function Not Found")
		(region (start 11 5) (end 11 8))
		(headline
			(reflow "The type")
			(reflow " ")
			(annotated code "X")
			(reflow " ")
			(reflow "does not have an associated function named")
			(reflow " ")
			(annotated code "f")
			(reflow "."))
		(document
			(source-region (file "type_redeclared_alias_then_nominal_assoc_lookup.md") (start 11 5) (end 11 8) (annotation error) (line-text "z = X.f(1) + 0")))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColonEqual,OpenSquare,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,CloseRound,OpPlus,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "X")
				(args))
			(ty (name "U64")))
		(s-type-decl
			(header (name "X")
				(args))
			(ty-tag-union
				(tags))
			(associated
				(s-decl
					(p-ident (raw "one"))
					(e-int (raw "1")))
				(s-decl
					(p-ident (raw "f"))
					(e-lambda
						(args
							(p-ident (raw "a")))
						(e-ident (raw "a"))))))
		(s-type-anno (name "y")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "y"))
			(e-ident (raw "X.one")))
		(s-decl
			(p-ident (raw "z"))
			(e-binop (op "+")
				(e-apply
					(e-ident (raw "X.f"))
					(e-int (raw "1")))
				(e-int (raw "0"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "y"))
		(e-runtime-error (tag "nested_value_not_found"))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(d-let
		(p-assign (ident "z"))
		(e-runtime-error (tag "erroneous_value_expr")
			(e-runtime-error (tag "erroneous_value_expr"))))
	(s-alias-decl
		(ty-header (name "X"))
		(ty-lookup (name "U64") (builtin)))
	(s-nominal-decl
		(ty-header (name "X"))
		(ty-tag-union)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64"))
		(patt (type "Error")))
	(type_decls
		(alias (type "X")
			(ty-header (name "X")))
		(nominal (type "X")
			(ty-header (name "X"))))
	(expressions
		(expr (type "U64"))
		(expr (type "Error"))))
~~~
