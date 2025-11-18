# META
~~~ini
description=Parameterized type alias with type variables
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

Pair(a, b) : (a, b)

swapPair : Pair(a, b) -> Pair(b, a)
swapPair = |(x, y)| (y, x)

main! = |_| swapPair(1, 2)
~~~
# EXPECTED
TYPE MISMATCH - type_alias_parameterized.md:8:13:8:27
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**type_alias_parameterized.md:8:13:8:27:**
```roc
main! = |_| swapPair(1, 2)
```
            ^^^^^^^^^^^^^^

It has the type:
    _Num(_size), Num(_size2) -> _ret_

But I expected it to be:
    _Pair(a, b) -> Pair(b, a)_

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpBar,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,
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
		(s-type-decl
			(header (name "Pair")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(ty-tuple
				(ty-var (raw "a"))
				(ty-var (raw "b"))))
		(s-type-anno (name "swapPair")
			(ty-fn
				(ty-apply
					(ty (name "Pair"))
					(ty-var (raw "a"))
					(ty-var (raw "b")))
				(ty-apply
					(ty (name "Pair"))
					(ty-var (raw "b"))
					(ty-var (raw "a")))))
		(s-decl
			(p-ident (raw "swapPair"))
			(e-lambda
				(args
					(p-tuple
						(p-ident (raw "x"))
						(p-ident (raw "y"))))
				(e-tuple
					(e-ident (raw "y"))
					(e-ident (raw "x")))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-ident (raw "swapPair"))
					(e-int (raw "1"))
					(e-int (raw "2")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "swapPair"))
		(e-lambda
			(args
				(p-tuple
					(patterns
						(p-assign (ident "x"))
						(p-assign (ident "y")))))
			(e-tuple
				(elems
					(e-lookup-local
						(p-assign (ident "y")))
					(e-lookup-local
						(p-assign (ident "x"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Pair") (local)
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))
				(ty-apply (name "Pair") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "b")))
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "swapPair")))
			(e-lambda
				(args
					(p-underscore))
				(e-call
					(e-lookup-local
						(p-assign (ident "swapPair")))
					(e-num (value "1"))
					(e-num (value "2"))))))
	(s-alias-decl
		(ty-header (name "Pair")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-tuple
			(ty-rigid-var-lookup (ty-rigid-var (name "a")))
			(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Pair(a, b) -> Pair(b, a)"))
		(patt (type "_arg -> _ret")))
	(type_decls
		(alias (type "Pair(a, b)")
			(ty-header (name "Pair")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b"))))))
	(expressions
		(expr (type "Pair(a, b) -> Pair(b, a)"))
		(expr (type "_arg -> _ret"))))
~~~
