# META
~~~ini
description=String literals with explicit type suffixes, single-line and multiline
type=file
~~~
# SOURCE
~~~roc
Tag := [Tag(List(U8))].{
	from_quote : List(U8) -> Try(Tag, [BadQuotedBytes(Str)])
	from_quote = |bytes| Ok(Tag(bytes))
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
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
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
						(ty-apply
							(ty (name "List"))
							(ty (name "U8"))))))
			(associated
				(s-type-anno (name "from_quote")
					(ty-fn
						(ty-apply
							(ty (name "List"))
							(ty (name "U8")))
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
							(p-ident (raw "bytes")))
						(e-apply
							(e-tag (raw "Ok"))
							(e-apply
								(e-tag (raw "Tag"))
								(e-ident (raw "bytes"))))))))
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
Tag := [Tag(List(U8))].{
	from_quote : List(U8) -> Try(Tag, [BadQuotedBytes(Str)])
	from_quote = |bytes| Ok(Tag(bytes))
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
				(p-assign (ident "bytes")))
			(e-tag (name "Ok")
				(args
					(e-tag (name "Tag")
						(args
							(e-lookup-local
								(p-assign (ident "bytes"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "U8") (builtin)))
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
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "U8") (builtin)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(U8) -> Try(Tag, [BadQuotedBytes(Str)])"))
		(patt (type "Tag"))
		(patt (type "Tag")))
	(type_decls
		(nominal (type "Tag")
			(ty-header (name "Tag"))))
	(expressions
		(expr (type "List(U8) -> Try(Tag, [BadQuotedBytes(Str)])"))
		(expr (type "Tag"))
		(expr (type "Tag"))))
~~~
