# META
~~~ini
description=Literal type suffixes may name a qualified type on every literal form
type=snippet
~~~
# SOURCE
~~~roc
Outer := [].{
	Amount := [Val(Dec)].{
		is_eq : _
		from_numeral : Numeral -> Try(Amount, [InvalidNumeral(Str)])
		from_numeral = |numeral| match Dec.from_numeral(numeral) {
			Ok(value) => Ok(Val(value))
			Err(err) => Err(err)
		}
	}
	Label := [Label(Str)].{
		from_quote : Str -> Try(Label, [BadQuotedBytes(Str)])
		from_quote = |str| Ok(Label(str))
	}
}

int = 0x05080a.Outer.Amount
frac = 2.5.Outer.Amount
codepoint = 'a'.Outer.Amount
single = "Roc".Outer.Label

multi =
	\\line one
	.Outer.Label

classify : Outer.Amount -> U8
classify = |amount| match amount {
	5.Outer.Amount => 1
	2.5.Outer.Amount => 2
	'a'.Outer.Amount => 3
	_ => 0
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,Underscore,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,Int,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,Float,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,SingleQuote,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,
MultilineStringStart,StringPart,
DotUpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
Int,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,OpFatArrow,Int,
Float,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,OpFatArrow,Int,
SingleQuote,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,OpFatArrow,Int,
Underscore,OpFatArrow,Int,
CloseCurly,
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
								(ty (name "Dec")))))
					(associated
						(s-type-anno (name "is_eq")
							(_))
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
										(e-ident (raw "Dec.from_numeral"))
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
												(e-ident (raw "err"))))))))))
				(s-type-decl
					(header (name "Label")
						(args))
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "Label"))
								(ty (name "Str")))))
					(associated
						(s-type-anno (name "from_quote")
							(ty-fn
								(ty (name "Str"))
								(ty-apply
									(ty (name "Try"))
									(ty (name "Label"))
									(ty-tag-union
										(tags
											(ty-apply
												(ty (name "BadQuotedBytes"))
												(ty (name "Str"))))))))
						(s-decl
							(p-ident (raw "from_quote"))
							(e-lambda
								(args
									(p-ident (raw "str")))
								(e-apply
									(e-tag (raw "Ok"))
									(e-apply
										(e-tag (raw "Label"))
										(e-ident (raw "str"))))))))))
		(s-decl
			(p-ident (raw "int"))
			(e-typed-int (raw "0x05080a") (type "Outer.Amount")))
		(s-decl
			(p-ident (raw "frac"))
			(e-typed-frac (raw "2.5") (type "Outer.Amount")))
		(s-decl
			(p-ident (raw "codepoint"))
			(e-single-quote (raw "'a'") (type "Outer.Amount")))
		(s-decl
			(p-ident (raw "single"))
			(e-typed-string (type "Outer.Label")
				(e-string-part (raw "Roc"))))
		(s-decl
			(p-ident (raw "multi"))
			(e-typed-multiline-string (type "Outer.Label")
				(e-string-part (raw "line one"))))
		(s-type-anno (name "classify")
			(ty-fn
				(ty (name "Outer.Amount"))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "classify"))
			(e-lambda
				(args
					(p-ident (raw "amount")))
				(e-match
					(e-ident (raw "amount"))
					(branches
						(branch
							(p-typed-int (raw "5") (type "Outer.Amount"))
							(e-int (raw "1")))
						(branch
							(p-typed-frac (raw "2.5") (type "Outer.Amount"))
							(e-int (raw "2")))
						(branch
							(p-single-quote (raw "'a'") (type "Outer.Amount"))
							(e-int (raw "3")))
						(branch
							(p-underscore)
							(e-int (raw "0")))))))))
~~~
# FORMATTED
~~~roc
Outer := [].{
	Amount := [Val(Dec)].{
		is_eq : _
		from_numeral : Numeral -> Try(Amount, [InvalidNumeral(Str)])
		from_numeral = |numeral| match Dec.from_numeral(numeral) {
			Ok(value) => Ok(Val(value))
			Err(err) => Err(err)
		}
	}
	Label := [Label(Str)].{
		from_quote : Str -> Try(Label, [BadQuotedBytes(Str)])
		from_quote = |str| Ok(Label(str))
	}
}

int = 0x05080a.Outer.Amount

frac = 2.5.Outer.Amount

codepoint = 'a'.Outer.Amount

single = "Roc".Outer.Label

multi =
	\\line one
	.Outer.Label

classify : Outer.Amount -> U8
classify = |amount| match amount {
	5.Outer.Amount => 1
	2.5.Outer.Amount => 2
	'a'.Outer.Amount => 3
	_ => 0
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "qualified_literal_type_suffixes.Outer.Amount.is_eq"))
		(e-derived-method (kind "equality"))
		(annotation
			(ty-underscore)))
	(d-let
		(p-assign (ident "qualified_literal_type_suffixes.Outer.Amount.from_numeral"))
		(e-lambda
			(args
				(p-assign (ident "numeral")))
			(e-match
				(match
					(cond
						(e-call (constraint-fn-var 384)
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
		(p-assign (ident "qualified_literal_type_suffixes.Outer.Label.from_quote"))
		(e-lambda
			(args
				(p-assign (ident "str")))
			(e-tag (name "Ok")
				(args
					(e-tag (name "Label")
						(args
							(e-lookup-local
								(p-assign (ident "str"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Label") (local))
					(ty-tag-union
						(ty-tag-name (name "BadQuotedBytes")
							(ty-lookup (name "Str") (builtin))))))))
	(d-let
		(p-assign (ident "int"))
		(e-typed-int (value "329738") (type "Outer.Amount")))
	(d-let
		(p-assign (ident "frac"))
		(e-typed-frac (value "2500000000000000000") (type "Outer.Amount")))
	(d-let
		(p-assign (ident "codepoint"))
		(e-typed-int (value "97") (type "Outer.Amount")))
	(d-let
		(p-assign (ident "single"))
		(e-string
			(e-literal (string "Roc"))))
	(d-let
		(p-assign (ident "multi"))
		(e-string
			(e-literal (string "line one"))))
	(d-let
		(p-assign (ident "classify"))
		(e-lambda
			(args
				(p-assign (ident "amount")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "amount"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-num (value "5"))))
							(value
								(e-num (value "1"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-small-dec)))
							(value
								(e-num (value "2"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-num (value "97"))))
							(value
								(e-num (value "3"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-underscore)))
							(value
								(e-num (value "0"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Outer.Amount") (local))
				(ty-lookup (name "U8") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Outer"))
		(ty-tag-union))
	(s-nominal-decl
		(ty-header (name "qualified_literal_type_suffixes.Outer.Amount"))
		(ty-tag-union
			(ty-tag-name (name "Val")
				(ty-lookup (name "Dec") (builtin)))))
	(s-nominal-decl
		(ty-header (name "qualified_literal_type_suffixes.Outer.Label"))
		(ty-tag-union
			(ty-tag-name (name "Label")
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_a"))
		(patt (type "Numeral -> Try(Outer.Amount, [InvalidNumeral(Str)])"))
		(patt (type "Str -> Try(Outer.Label, [BadQuotedBytes(Str)])"))
		(patt (type "Outer.Amount"))
		(patt (type "Outer.Amount"))
		(patt (type "Outer.Amount"))
		(patt (type "Outer.Label"))
		(patt (type "Outer.Label"))
		(patt (type "Outer.Amount -> U8")))
	(type_decls
		(nominal (type "Outer")
			(ty-header (name "Outer")))
		(nominal (type "Outer.Amount")
			(ty-header (name "qualified_literal_type_suffixes.Outer.Amount")))
		(nominal (type "Outer.Label")
			(ty-header (name "qualified_literal_type_suffixes.Outer.Label"))))
	(expressions
		(expr (type "_a"))
		(expr (type "Numeral -> Try(Outer.Amount, [InvalidNumeral(Str)])"))
		(expr (type "Str -> Try(Outer.Label, [BadQuotedBytes(Str)])"))
		(expr (type "Outer.Amount"))
		(expr (type "Outer.Amount"))
		(expr (type "Outer.Amount"))
		(expr (type "Outer.Label"))
		(expr (type "Outer.Label"))
		(expr (type "Outer.Amount -> U8"))))
~~~
