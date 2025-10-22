# META
~~~ini
description=Nested instantiation with record field access causing type mismatch
type=file
~~~
# SOURCE
~~~roc
app [] { pf: platform "../basic-cli/platform.roc" }
# TODO: if you do this whole thing as an expr block, with `composed` at
# the end instead of `answer =`, it triggers a parser bug!

make_record : a -> { value: a, tag: Str }
make_record = |x| { value: x, tag: "data" }

get_value : { value: a, tag: Str } -> a
get_value = |r| r.value

composed : List(a) -> Str
composed = |n| get_value(make_record(n))

answer = composed([42])
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenSquare,Int,CloseSquare,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides)
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-anno (name "make_record")
			(ty-fn
				(ty-var (raw "a"))
				(ty-record
					(anno-record-field (name "value")
						(ty-var (raw "a")))
					(anno-record-field (name "tag")
						(ty (name "Str"))))))
		(s-decl
			(p-ident (raw "make_record"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-record
					(field (field "value")
						(e-ident (raw "x")))
					(field (field "tag")
						(e-string
							(e-string-part (raw "data")))))))
		(s-type-anno (name "get_value")
			(ty-fn
				(ty-record
					(anno-record-field (name "value")
						(ty-var (raw "a")))
					(anno-record-field (name "tag")
						(ty (name "Str"))))
				(ty-var (raw "a"))))
		(s-decl
			(p-ident (raw "get_value"))
			(e-lambda
				(args
					(p-ident (raw "r")))
				(e-field-access
					(e-ident (raw "r"))
					(e-ident (raw "value")))))
		(s-type-anno (name "composed")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "a")))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "composed"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-apply
					(e-ident (raw "get_value"))
					(e-apply
						(e-ident (raw "make_record"))
						(e-ident (raw "n"))))))
		(s-decl
			(p-ident (raw "answer"))
			(e-apply
				(e-ident (raw "composed"))
				(e-list
					(e-int (raw "42")))))))
~~~
# FORMATTED
~~~roc
app [] { pf: platform "../basic-cli/platform.roc" }
# TODO: if you do this whole thing as an expr block, with `composed` at
# the end instead of `answer =`, it triggers a parser bug!

make_record : a -> { value : a, tag : Str }
make_record = |x| { value: x, tag: "data" }

get_value : { value : a, tag : Str } -> a
get_value = |r| r.value

composed : List(a) -> Str
composed = |n| get_value(make_record(n))

answer = composed([42])
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "make_record"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-record
				(fields
					(field (name "value")
						(e-lookup-local
							(p-assign (ident "x"))))
					(field (name "tag")
						(e-string
							(e-literal (string "data")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-record
					(field (field "value")
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(field (field "tag")
						(ty-lookup (name "Str") (external-module "Str")))))))
	(d-let
		(p-assign (ident "get_value"))
		(e-lambda
			(args
				(p-assign (ident "r")))
			(e-dot-access (field "value")
				(receiver
					(e-lookup-local
						(p-assign (ident "r"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-record
					(field (field "value")
						(ty-rigid-var (name "a")))
					(field (field "tag")
						(ty-lookup (name "Str") (external-module "Str"))))
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))
	(d-let
		(p-assign (ident "composed"))
		(e-closure
			(captures
				(capture (ident "get_value"))
				(capture (ident "make_record")))
			(e-lambda
				(args
					(p-assign (ident "n")))
				(e-call
					(e-lookup-local
						(p-assign (ident "get_value")))
					(e-call
						(e-lookup-local
							(p-assign (ident "make_record")))
						(e-lookup-local
							(p-assign (ident "n")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-rigid-var (name "a")))
				(ty-lookup (name "Str") (external-module "Str")))))
	(d-let
		(p-assign (ident "answer"))
		(e-call
			(e-lookup-local
				(p-assign (ident "composed")))
			(e-list
				(elems
					(e-num (value "42")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> { tag: Error, value: a }"))
		(patt (type "{ tag: Error, value: a } -> a"))
		(patt (type "List(a) -> Error"))
		(patt (type "Error")))
	(expressions
		(expr (type "a -> { tag: Error, value: a }"))
		(expr (type "{ tag: Error, value: a } -> a"))
		(expr (type "List(a) -> Error"))
		(expr (type "Error"))))
~~~
