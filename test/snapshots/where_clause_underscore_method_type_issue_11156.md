# META
~~~ini
description=An underscore standing in for a whole where-clause method type is inferred from the body's use of that method (issue 11156)
type=snippet
~~~
# SOURCE
~~~roc
Blub := {}.{
	parse : Str -> Try(a, [ParseError]) where [a.parser_for : _]
	parse = |str| Json.parse(str).map_err(|_| ParseError)
}

main : Try({ hi : Dec }, [ParseError])
main = Blub.parse("{ \"hi\": 4 }")
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,Underscore,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpBar,Underscore,OpBar,UpperIdent,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Blub")
				(args))
			(ty-record)
			(associated
				(s-type-anno (name "parse")
					(ty-fn
						(ty (name "Str"))
						(ty-apply
							(ty (name "Try"))
							(ty-var (raw "a"))
							(ty-tag-union
								(tags
									(ty (name "ParseError"))))))
					(where
						(method (mod-of "a") (name "parser_for")
							(_))))
				(s-decl
					(p-ident (raw "parse"))
					(e-lambda
						(args
							(p-ident (raw "str")))
						(e-method-call (method ".map_err")
							(receiver
								(e-apply
									(e-ident (raw "Json.parse"))
									(e-ident (raw "str"))))
							(args
								(e-lambda
									(args
										(p-underscore))
									(e-tag (raw "ParseError")))))))))
		(s-type-anno (name "main")
			(ty-apply
				(ty (name "Try"))
				(ty-record
					(anno-record-field (name "hi")
						(ty (name "Dec"))))
				(ty-tag-union
					(tags
						(ty (name "ParseError"))))))
		(s-decl
			(p-ident (raw "main"))
			(e-apply
				(e-ident (raw "Blub.parse"))
				(e-string
					(e-string-part (raw "{ \"hi\": 4 }")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "where_clause_underscore_method_type_issue_11156.Blub.parse"))
		(e-lambda
			(args
				(p-assign (ident "str")))
			(e-dispatch-call (method "map_err") (constraint-fn-var 296)
				(receiver
					(e-call (constraint-fn-var 294)
						(e-lookup-external
							(builtin))
						(e-lookup-local
							(p-assign (ident "str")))))
				(args
					(e-lambda
						(args
							(p-underscore))
						(e-tag (name "ParseError"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-rigid-var (name "a"))
					(ty-tag-union
						(ty-tag-name (name "ParseError")))))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "parser_for")
					(ty-underscore)))))
	(d-let
		(p-assign (ident "main"))
		(e-call (constraint-fn-var 350)
			(e-lookup-local
				(p-assign (ident "where_clause_underscore_method_type_issue_11156.Blub.parse")))
			(e-string
				(e-literal (string "{ "hi": 4 }"))))
		(annotation
			(ty-apply (name "Try") (builtin)
				(ty-record
					(field (field "hi")
						(ty-lookup (name "Dec") (builtin))))
				(ty-tag-union
					(ty-tag-name (name "ParseError"))))))
	(s-nominal-decl
		(ty-header (name "Blub"))
		(ty-record)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Try(a, [ParseError]) where [a.parser_for : Encoding.JsonEncoding -> (Encoding.JsonState -> Try({ rest: Encoding.JsonState, value: a }, [InvalidJson(Str), ..errs]))]"))
		(patt (type "Try({ hi: Dec }, [ParseError])")))
	(type_decls
		(nominal (type "Blub")
			(ty-header (name "Blub"))))
	(expressions
		(expr (type "Str -> Try(a, [ParseError]) where [a.parser_for : Encoding.JsonEncoding -> (Encoding.JsonState -> Try({ rest: Encoding.JsonState, value: a }, [InvalidJson(Str), ..errs]))]"))
		(expr (type "Try({ hi: Dec }, [ParseError])"))))
~~~
