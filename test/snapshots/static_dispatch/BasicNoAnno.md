# META
~~~ini
description=
type=file
~~~
# SOURCE
~~~roc
BasicNoAnno := [Val(Str)].{
  to_str = |BasicNoAnno.Val(s)| s

  to_str2 = |test| test.to_str()
}

helper1 = |x| x.to_str()

helper2 = |x| x.to_str2()

val = BasicNoAnno.Val("hello")

main : (Str, Str)
main = (helper1(val), helper2(val))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpColon,OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpenRound,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "BasicNoAnno")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Val"))
						(ty (name "Str")))))
			(associated
				(s-decl
					(p-ident (raw "to_str"))
					(e-lambda
						(args
							(p-tag (raw ".Val")
								(p-ident (raw "s"))))
						(e-ident (raw "s"))))
				(s-decl
					(p-ident (raw "to_str2"))
					(e-lambda
						(args
							(p-ident (raw "test")))
						(e-field-access
							(e-ident (raw "test"))
							(e-apply
								(e-ident (raw "to_str"))))))))
		(s-decl
			(p-ident (raw "helper1"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-field-access
					(e-ident (raw "x"))
					(e-apply
						(e-ident (raw "to_str"))))))
		(s-decl
			(p-ident (raw "helper2"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-field-access
					(e-ident (raw "x"))
					(e-apply
						(e-ident (raw "to_str2"))))))
		(s-decl
			(p-ident (raw "val"))
			(e-apply
				(e-tag (raw "BasicNoAnno.Val"))
				(e-string
					(e-string-part (raw "hello")))))
		(s-type-anno (name "main")
			(ty-tuple
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "main"))
			(e-tuple
				(e-apply
					(e-ident (raw "helper1"))
					(e-ident (raw "val")))
				(e-apply
					(e-ident (raw "helper2"))
					(e-ident (raw "val")))))))
~~~
# FORMATTED
~~~roc
BasicNoAnno := [Val(Str)].{
	to_str = |BasicNoAnno.Val(s)| s
	to_str2 = |test| test.to_str()
}

helper1 = |x| x.to_str()

helper2 = |x| x.to_str2()

val = BasicNoAnno.Val("hello")

main : (Str, Str)
main = (helper1(val), helper2(val))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "helper1"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-dot-access (field "to_str")
				(receiver
					(e-lookup-local
						(p-assign (ident "x"))))
				(args))))
	(d-let
		(p-assign (ident "helper2"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-dot-access (field "to_str2")
				(receiver
					(e-lookup-local
						(p-assign (ident "x"))))
				(args))))
	(d-let
		(p-assign (ident "val"))
		(e-nominal (nominal "BasicNoAnno")
			(e-tag (name "Val")
				(args
					(e-string
						(e-literal (string "hello")))))))
	(d-let
		(p-assign (ident "main"))
		(e-tuple
			(elems
				(e-call
					(e-lookup-local
						(p-assign (ident "helper1")))
					(e-lookup-local
						(p-assign (ident "val"))))
				(e-call
					(e-lookup-local
						(p-assign (ident "helper2")))
					(e-lookup-local
						(p-assign (ident "val"))))))
		(annotation
			(ty-tuple
				(ty-lookup (name "Str") (external-module "Str"))
				(ty-lookup (name "Str") (external-module "Str")))))
	(d-let
		(p-assign (ident "BasicNoAnno.to_str"))
		(e-closure
			(captures
				(capture (ident "s")))
			(e-lambda
				(args
					(p-nominal
						(p-applied-tag)))
				(e-lookup-local
					(p-assign (ident "s"))))))
	(d-let
		(p-assign (ident "BasicNoAnno.to_str2"))
		(e-lambda
			(args
				(p-assign (ident "test")))
			(e-dot-access (field "to_str")
				(receiver
					(e-lookup-local
						(p-assign (ident "test"))))
				(args))))
	(s-nominal-decl
		(ty-header (name "BasicNoAnno"))
		(ty-tag-union
			(ty-tag-name (name "Val")
				(ty-lookup (name "Str") (external-module "Str"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> b where [a.to_str : a -> b]"))
		(patt (type "a -> b where [a.to_str2 : a -> b]"))
		(patt (type "BasicNoAnno"))
		(patt (type "(Str, Str)"))
		(patt (type "BasicNoAnno -> Str"))
		(patt (type "a -> b where [a.to_str : a -> b]")))
	(type_decls
		(nominal (type "BasicNoAnno")
			(ty-header (name "BasicNoAnno"))))
	(expressions
		(expr (type "a -> b where [a.to_str : a -> b]"))
		(expr (type "a -> b where [a.to_str2 : a -> b]"))
		(expr (type "BasicNoAnno"))
		(expr (type "(Str, Str)"))
		(expr (type "BasicNoAnno -> Str"))
		(expr (type "a -> b where [a.to_str : a -> b]"))))
~~~
