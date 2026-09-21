# META
~~~ini
description=A literal type suffix naming a where alias is rejected like a where alias in a type annotation
type=snippet
~~~
# SOURCE
~~~roc
Probe := [].{
	a.Num : where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]
	a.Quote : where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]
}

a.LocalNum : where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]

unqualified = 5.LocalNum
qualified = 5.Probe.Num
qualified_frac = 2.5.Probe.Num
qualified_string = "hi".Probe.Quote
~~~
# EXPECTED
WHERE ALIAS USED AS A TYPE - literal_type_suffix_where_alias.md:8:15:8:25
WHERE ALIAS USED AS A TYPE - literal_type_suffix_where_alias.md:9:13:9:24
WHERE ALIAS USED AS A TYPE - literal_type_suffix_where_alias.md:10:18:10:31
WHERE ALIAS USED AS A TYPE - literal_type_suffix_where_alias.md:11:20:11:36
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Where Alias Used as a Type")
		(region (start 8 15) (end 8 25))
		(headline
			(annotated type "LocalNum")
			(reflow " ")
			(reflow "is a where alias, not a type."))
		(document
			(source-region (file "literal_type_suffix_where_alias.md") (start 8 15) (end 8 25) (annotation error) (line-text "unqualified = 5.LocalNum"))
			(line-break)
			(reflow "A where alias names a set of method constraints, so it constrains a type variable in a")
			(reflow " ")
			(annotated code "where")
			(reflow " ")
			(reflow "clause rather than standing in for a type of its own.")))
	(report
		(severity runtime_error)
		(title "Where Alias Used as a Type")
		(region (start 9 13) (end 9 24))
		(headline
			(annotated type "Probe.Num")
			(reflow " ")
			(reflow "is a where alias, not a type."))
		(document
			(source-region (file "literal_type_suffix_where_alias.md") (start 9 13) (end 9 24) (annotation error) (line-text "qualified = 5.Probe.Num"))
			(line-break)
			(reflow "A where alias names a set of method constraints, so it constrains a type variable in a")
			(reflow " ")
			(annotated code "where")
			(reflow " ")
			(reflow "clause rather than standing in for a type of its own.")))
	(report
		(severity runtime_error)
		(title "Where Alias Used as a Type")
		(region (start 10 18) (end 10 31))
		(headline
			(annotated type "Probe.Num")
			(reflow " ")
			(reflow "is a where alias, not a type."))
		(document
			(source-region (file "literal_type_suffix_where_alias.md") (start 10 18) (end 10 31) (annotation error) (line-text "qualified_frac = 2.5.Probe.Num"))
			(line-break)
			(reflow "A where alias names a set of method constraints, so it constrains a type variable in a")
			(reflow " ")
			(annotated code "where")
			(reflow " ")
			(reflow "clause rather than standing in for a type of its own.")))
	(report
		(severity runtime_error)
		(title "Where Alias Used as a Type")
		(region (start 11 20) (end 11 36))
		(headline
			(annotated type "Probe.Quote")
			(reflow " ")
			(reflow "is a where alias, not a type."))
		(document
			(source-region (file "literal_type_suffix_where_alias.md") (start 11 20) (end 11 36) (annotation error) (line-text "qualified_string = \"hi\".Probe.Quote"))
			(line-break)
			(reflow "A where alias names a set of method constraints, so it constrains a type variable in a")
			(reflow " ")
			(annotated code "where")
			(reflow " ")
			(reflow "clause rather than standing in for a type of its own."))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,CloseSquare,Dot,OpenCurly,
LowerIdent,NoSpaceDotUpperIdent,OpColon,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,CloseSquare,
LowerIdent,NoSpaceDotUpperIdent,OpColon,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,CloseSquare,
CloseCurly,
LowerIdent,NoSpaceDotUpperIdent,OpColon,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,CloseSquare,
LowerIdent,OpAssign,Int,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,Int,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,Float,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Probe")
				(args))
			(ty-tag-union
				(tags))
			(associated
				(s-type-decl
					(header (name ".Num")
						(args))
					(ty-var (raw "a"))
					(where
						(method (mod-of "a") (name "from_numeral")
							(ty-fn
								(ty (name "Numeral"))
								(ty-apply
									(ty (name "Try"))
									(ty-var (raw "a"))
									(ty-tag-union
										(tags
											(ty-apply
												(ty (name "InvalidNumeral"))
												(ty (name "Str"))))))))))
				(s-type-decl
					(header (name ".Quote")
						(args))
					(ty-var (raw "a"))
					(where
						(method (mod-of "a") (name "from_quote")
							(ty-fn
								(ty (name "Str"))
								(ty-apply
									(ty (name "Try"))
									(ty-var (raw "a"))
									(ty-tag-union
										(tags
											(ty-apply
												(ty (name "BadQuotedBytes"))
												(ty (name "Str"))))))))))))
		(s-type-decl
			(header (name ".LocalNum")
				(args))
			(ty-var (raw "a"))
			(where
				(method (mod-of "a") (name "from_numeral")
					(ty-fn
						(ty (name "Numeral"))
						(ty-apply
							(ty (name "Try"))
							(ty-var (raw "a"))
							(ty-tag-union
								(tags
									(ty-apply
										(ty (name "InvalidNumeral"))
										(ty (name "Str"))))))))))
		(s-decl
			(p-ident (raw "unqualified"))
			(e-typed-int (raw "5") (type "LocalNum")))
		(s-decl
			(p-ident (raw "qualified"))
			(e-typed-int (raw "5") (type "Probe.Num")))
		(s-decl
			(p-ident (raw "qualified_frac"))
			(e-typed-frac (raw "2.5") (type "Probe.Num")))
		(s-decl
			(p-ident (raw "qualified_string"))
			(e-typed-string (type "Probe.Quote")
				(e-string-part (raw "hi"))))))
~~~
# FORMATTED
~~~roc
Probe := [].{
	a.Num :  where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]
	a.Quote :  where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]
}

a.LocalNum :  where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]

unqualified = 5.LocalNum

qualified = 5.Probe.Num

qualified_frac = 2.5.Probe.Num

qualified_string = "hi".Probe.Quote
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "unqualified"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "qualified"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "qualified_frac"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "qualified_string"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(s-nominal-decl
		(ty-header (name "Probe"))
		(ty-tag-union))
	(s-where-alias-decl
		(ty-header (name "literal_type_suffix_where_alias.Probe.Num"))
		(ty-rigid-var (name "a"))
		(where
			(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "from_numeral")
				(ty-fn (effectful false)
					(ty-lookup (name "Numeral") (builtin))
					(ty-apply (name "Try") (builtin)
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-tag-union
							(ty-tag-name (name "InvalidNumeral")
								(ty-lookup (name "Str") (builtin)))))))))
	(s-where-alias-decl
		(ty-header (name "literal_type_suffix_where_alias.Probe.Quote"))
		(ty-rigid-var (name "a"))
		(where
			(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "from_quote")
				(ty-fn (effectful false)
					(ty-lookup (name "Str") (builtin))
					(ty-apply (name "Try") (builtin)
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-tag-union
							(ty-tag-name (name "BadQuotedBytes")
								(ty-lookup (name "Str") (builtin)))))))))
	(s-where-alias-decl
		(ty-header (name "LocalNum"))
		(ty-rigid-var (name "a"))
		(where
			(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "from_numeral")
				(ty-fn (effectful false)
					(ty-lookup (name "Numeral") (builtin))
					(ty-apply (name "Try") (builtin)
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-tag-union
							(ty-tag-name (name "InvalidNumeral")
								(ty-lookup (name "Str") (builtin))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error")))
	(type_decls
		(nominal (type "Probe")
			(ty-header (name "Probe")))
		(where-alias (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")
			(ty-header (name "literal_type_suffix_where_alias.Probe.Num")))
		(where-alias (type "a where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]")
			(ty-header (name "literal_type_suffix_where_alias.Probe.Quote")))
		(where-alias (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")
			(ty-header (name "LocalNum"))))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
