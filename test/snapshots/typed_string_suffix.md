# META
~~~ini
description=String literals with explicit type suffixes, single-line and multiline
type=file
~~~
# SOURCE
~~~roc
Tag := [Tag(Str)].{
	from_quote : Str -> Try(Tag, [BadQuotedBytes(Str)])
	from_quote = |str| Ok(Tag(str))
}

single = "Roc".Tag

multi =
	\\line one
	\\line two
	.Tag
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
CloseCurly,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,
MultilineStringStart,StringPart,
MultilineStringStart,StringPart,
DotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Tag")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Tag"))
						(ty (name "Str")))))
			(associated
				(s-type-anno (name "from_quote")
					(ty-fn
						(ty (name "Str"))
						(ty-apply
							(ty (name "Try"))
							(ty (name "Tag"))
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
								(e-tag (raw "Tag"))
								(e-ident (raw "str"))))))))
		(s-decl
			(p-ident (raw "single"))
			(e-typed-string (type "Tag")
				(e-string-part (raw "Roc"))))
		(s-decl
			(p-ident (raw "multi"))
			(e-typed-multiline-string (type "Tag")
				(e-string-part (raw "line one"))
				(e-string-part (raw "line two"))))))
~~~
# FORMATTED
~~~roc
Tag := [Tag(Str)].{
	from_quote : Str -> Try(Tag, [BadQuotedBytes(Str)])
	from_quote = |str| Ok(Tag(str))
}

single = "Roc".Tag

multi = 
	\\line one
	\\line two
	.Tag
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "typed_string_suffix.Tag.from_quote"))
		(e-lambda
			(args
				(p-assign (ident "str")))
			(e-tag (name "Ok")
				(args
					(e-tag (name "Tag")
						(args
							(e-lookup-local
								(p-assign (ident "str"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Tag") (local))
					(ty-tag-union
						(ty-tag-name (name "BadQuotedBytes")
							(ty-lookup (name "Str") (builtin))))))))
	(d-let
		(p-assign (ident "single"))
		(e-string
			(e-literal (string "Roc"))))
	(d-let
		(p-assign (ident "multi"))
		(e-string
			(e-literal (string "line one
line two"))))
	(s-nominal-decl
		(ty-header (name "Tag"))
		(ty-tag-union
			(ty-tag-name (name "Tag")
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Try(Tag, [BadQuotedBytes(Str)])"))
		(patt (type "Tag"))
		(patt (type "Tag")))
	(type_decls
		(nominal (type "Tag")
			(ty-header (name "Tag"))))
	(expressions
		(expr (type "Str -> Try(Tag, [BadQuotedBytes(Str)])"))
		(expr (type "Tag"))
		(expr (type "Tag"))))
~~~
