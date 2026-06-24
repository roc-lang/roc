# META
~~~ini
description=underscore_in_assignment_pattern
type=snippet
~~~
# SOURCE
~~~roc
import Module exposing [Pair]

Pair1(x, _) = Pair(0, 1)
Pair2(_, y) = Pair(0, 1)
Pair3(_, _) = Pair(0, 1)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwImport,UpperIdent,KwExposing,OpenSquare,UpperIdent,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,Underscore,CloseRound,OpAssign,UpperIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,
UpperIdent,NoSpaceOpenRound,Underscore,Comma,LowerIdent,CloseRound,OpAssign,UpperIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,
UpperIdent,NoSpaceOpenRound,Underscore,Comma,Underscore,CloseRound,OpAssign,UpperIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "Module")
			(exposing
				(exposed-upper-ident (text "Pair"))))
		(s-decl
			(p-tag (raw "Pair1")
				(p-ident (raw "x"))
				(p-underscore))
			(e-apply
				(e-tag (raw "Pair"))
				(e-int (raw "0"))
				(e-int (raw "1"))))
		(s-decl
			(p-tag (raw "Pair2")
				(p-underscore)
				(p-ident (raw "y")))
			(e-apply
				(e-tag (raw "Pair"))
				(e-int (raw "0"))
				(e-int (raw "1"))))
		(s-decl
			(p-tag (raw "Pair3")
				(p-underscore)
				(p-underscore))
			(e-apply
				(e-tag (raw "Pair"))
				(e-int (raw "0"))
				(e-int (raw "1"))))))
~~~
# FORMATTED
~~~roc
import Module exposing [Pair]

Pair1(x, _) = Pair(0, 1)

Pair2(_, y) = Pair(0, 1)

Pair3(_, _) = Pair(0, 1)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-applied-tag)
		(e-tag (name "Pair")
			(args
				(e-num (value "0"))
				(e-num (value "1")))))
	(d-let
		(p-applied-tag)
		(e-tag (name "Pair")
			(args
				(e-num (value "0"))
				(e-num (value "1")))))
	(d-let
		(p-applied-tag)
		(e-tag (name "Pair")
			(args
				(e-num (value "0"))
				(e-num (value "1")))))
	(s-import (module "Module")
		(exposes
			(exposed (name "Pair") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions
		(expr (type "[Pair(Dec, Dec), Pair1([], []), ..]"))
		(expr (type "[Pair(Dec, Dec), Pair2([], []), ..]"))
		(expr (type "[Pair(Dec, Dec), Pair3([], []), ..]"))))
~~~
