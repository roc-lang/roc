# META
~~~ini
description=Type application with variables in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

mapList : List(a), (a -> b) -> List(b)
mapList = |list, fn| list.map(fn)

main! = |_| mapList([1,2,3,4,5])
~~~
# EXPECTED
TYPE MISMATCH - type_app_with_vars.md:6:13:6:33
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**type_app_with_vars.md:6:13:6:33:**
```roc
main! = |_| mapList([1,2,3,4,5])
```
            ^^^^^^^^^^^^^^^^^^^^

It has the type:
    _List(Num(_size)) -> _ret_

But I expected it to be:
    _List(a), a -> b -> List(b)_

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,NoSpaceOpenRound,OpenSquare,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,CloseSquare,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/main.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno (name "mapList")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "a")))
				(ty-fn
					(ty-var (raw "a"))
					(ty-var (raw "b")))
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "b")))))
		(s-decl
			(p-ident (raw "mapList"))
			(e-lambda
				(args
					(p-ident (raw "list"))
					(p-ident (raw "fn")))
				(e-field-access
					(e-ident (raw "list"))
					(e-apply
						(e-ident (raw "map"))
						(e-ident (raw "fn"))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-ident (raw "mapList"))
					(e-list
						(e-int (raw "1"))
						(e-int (raw "2"))
						(e-int (raw "3"))
						(e-int (raw "4"))
						(e-int (raw "5"))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

mapList : List(a), (a -> b) -> List(b)
mapList = |list, fn| list.map(fn)

main! = |_| mapList([1, 2, 3, 4, 5])
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "mapList"))
		(e-lambda
			(args
				(p-assign (ident "list"))
				(p-assign (ident "fn")))
			(e-dot-access (field "map")
				(receiver
					(e-lookup-local
						(p-assign (ident "list"))))
				(args
					(e-lookup-local
						(p-assign (ident "fn"))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-apply (name "List") (builtin)
						(ty-rigid-var (name "a")))
					(ty-parens
						(ty-fn (effectful false)
							(ty-rigid-var-lookup (ty-rigid-var (name "a")))
							(ty-rigid-var (name "b"))))
					(ty-apply (name "List") (builtin)
						(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "mapList")))
			(e-lambda
				(args
					(p-underscore))
				(e-call
					(e-lookup-local
						(p-assign (ident "mapList")))
					(e-list
						(elems
							(e-num (value "1"))
							(e-num (value "2"))
							(e-num (value "3"))
							(e-num (value "4"))
							(e-num (value "5")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(a), a -> b -> List(b)"))
		(patt (type "_arg -> _ret")))
	(expressions
		(expr (type "List(a), a -> b -> List(b)"))
		(expr (type "_arg -> _ret"))))
~~~
