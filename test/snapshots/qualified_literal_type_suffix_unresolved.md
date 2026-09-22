# META
~~~ini
description=Qualified literal type suffixes that do not name a type report the qualified lookup's diagnostic
type=snippet
~~~
# SOURCE
~~~roc
Outer := [].{
	Amount := [Val(U32)].{
		from_numeral : Numeral -> Try(Amount, [InvalidNumeral(Str)])
		from_numeral = |numeral| match U32.from_numeral(numeral) {
			Ok(value) => Ok(Val(value))
			Err(err) => Err(err)
		}
	}
}

missing_nested = 5.Outer.Missing
missing_qualifier = 5.Nowhere.Amount
missing_string = "Roc".Outer.Missing
~~~
# EXPECTED
MISSING NESTED TYPE - qualified_literal_type_suffix_unresolved.md:11:18:11:33
MOD NOT IMPORTED - qualified_literal_type_suffix_unresolved.md:12:21:12:37
MISSING NESTED TYPE - qualified_literal_type_suffix_unresolved.md:13:18:13:37
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Nested Type")
		(region (start 11 18) (end 11 33))
		(headline
			(annotated code "Outer")
			(reflow " is in scope, but it doesn't have a nested type ")
			(reflow "named ")
			(annotated code "Missing")
			(reflow "."))
		(document
			(source-region (file "qualified_literal_type_suffix_unresolved.md") (start 11 18) (end 11 33) (annotation error) (line-text "missing_nested = 5.Outer.Missing"))))
	(report
		(severity runtime_error)
		(title "Mod Not Imported")
		(region (start 12 21) (end 12 37))
		(headline
			(text "There is no mod with the name ")
			(annotated code "Nowhere")
			(reflow " imported into this Roc file."))
		(document
			(source-region (file "qualified_literal_type_suffix_unresolved.md") (start 12 21) (end 12 37) (annotation error) (line-text "missing_qualifier = 5.Nowhere.Amount"))))
	(report
		(severity runtime_error)
		(title "Missing Nested Type")
		(region (start 13 18) (end 13 37))
		(headline
			(annotated code "Outer")
			(reflow " is in scope, but it doesn't have a nested type ")
			(reflow "named ")
			(annotated code "Missing")
			(reflow "."))
		(document
			(source-region (file "qualified_literal_type_suffix_unresolved.md") (start 13 18) (end 13 37) (annotation error) (line-text "missing_string = \"Roc\".Outer.Missing")))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,Int,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,Int,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Outer")
				(args))
			(ty-tag-union
				(tags))
			(associated
				(s-type-decl
					(header (name "Amount")
						(args))
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "Val"))
								(ty (name "U32")))))
					(associated
						(s-type-anno (name "from_numeral")
							(ty-fn
								(ty (name "Numeral"))
								(ty-apply
									(ty (name "Try"))
									(ty (name "Amount"))
									(ty-tag-union
										(tags
											(ty-apply
												(ty (name "InvalidNumeral"))
												(ty (name "Str"))))))))
						(s-decl
							(p-ident (raw "from_numeral"))
							(e-lambda
								(args
									(p-ident (raw "numeral")))
								(e-match
									(e-apply
										(e-ident (raw "U32.from_numeral"))
										(e-ident (raw "numeral")))
									(branches
										(branch
											(p-tag (raw "Ok")
												(p-ident (raw "value")))
											(e-apply
												(e-tag (raw "Ok"))
												(e-apply
													(e-tag (raw "Val"))
													(e-ident (raw "value")))))
										(branch
											(p-tag (raw "Err")
												(p-ident (raw "err")))
											(e-apply
												(e-tag (raw "Err"))
												(e-ident (raw "err"))))))))))))
		(s-decl
			(p-ident (raw "missing_nested"))
			(e-typed-int (raw "5") (type "Outer.Missing")))
		(s-decl
			(p-ident (raw "missing_qualifier"))
			(e-typed-int (raw "5") (type "Nowhere.Amount")))
		(s-decl
			(p-ident (raw "missing_string"))
			(e-typed-string (type "Outer.Missing")
				(e-string-part (raw "Roc"))))))
~~~
# FORMATTED
~~~roc
Outer := [].{
	Amount := [Val(U32)].{
		from_numeral : Numeral -> Try(Amount, [InvalidNumeral(Str)])
		from_numeral = |numeral| match U32.from_numeral(numeral) {
			Ok(value) => Ok(Val(value))
			Err(err) => Err(err)
		}
	}
}

missing_nested = 5.Outer.Missing

missing_qualifier = 5.Nowhere.Amount

missing_string = "Roc".Outer.Missing
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "qualified_literal_type_suffix_unresolved.Outer.Amount.from_numeral"))
		(e-lambda
			(args
				(p-assign (ident "numeral")))
			(e-match
				(match
					(cond
						(e-call (constraint-fn-var 303)
							(e-lookup-external
								(builtin))
							(e-lookup-local
								(p-assign (ident "numeral")))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-tag (name "Ok")
									(args
										(e-tag (name "Val")
											(args
												(e-lookup-local
													(p-assign (ident "value")))))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-tag (name "Err")
									(args
										(e-lookup-local
											(p-assign (ident "err")))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Numeral") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Amount") (local))
					(ty-tag-union
						(ty-tag-name (name "InvalidNumeral")
							(ty-lookup (name "Str") (builtin))))))))
	(d-let
		(p-assign (ident "missing_nested"))
		(e-runtime-error (tag "nested_type_not_found")))
	(d-let
		(p-assign (ident "missing_qualifier"))
		(e-runtime-error (tag "mod_not_imported")))
	(d-let
		(p-assign (ident "missing_string"))
		(e-runtime-error (tag "nested_type_not_found")))
	(s-nominal-decl
		(ty-header (name "Outer"))
		(ty-tag-union))
	(s-nominal-decl
		(ty-header (name "qualified_literal_type_suffix_unresolved.Outer.Amount"))
		(ty-tag-union
			(ty-tag-name (name "Val")
				(ty-lookup (name "U32") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Numeral -> Try(Outer.Amount, [InvalidNumeral(Str)])"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error")))
	(type_decls
		(nominal (type "Outer")
			(ty-header (name "Outer")))
		(nominal (type "Outer.Amount")
			(ty-header (name "qualified_literal_type_suffix_unresolved.Outer.Amount"))))
	(expressions
		(expr (type "Numeral -> Try(Outer.Amount, [InvalidNumeral(Str)])"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
