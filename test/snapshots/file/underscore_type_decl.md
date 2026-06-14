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
NON-EXHAUSTIVE DESTRUCTURE - underscore_type_decl.md:3:1:3:12
NON-EXHAUSTIVE DESTRUCTURE - underscore_type_decl.md:4:1:4:12
NON-EXHAUSTIVE DESTRUCTURE - underscore_type_decl.md:5:1:5:12
# PROBLEMS
**NON-EXHAUSTIVE DESTRUCTURE**
This destructuring pattern doesn't cover all possible cases:
**underscore_type_decl.md:3:1:3:12:**
```roc
Pair1(x, _) = Pair(0, 1)
```
^^^^^^^^^^^

The value being destructured has type:
        _[Pair(a, b), Pair1([], []), ..]
  where [
    a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),
    b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]),
  ]_

Missing patterns:
        _


**NON-EXHAUSTIVE DESTRUCTURE**
This destructuring pattern doesn't cover all possible cases:
**underscore_type_decl.md:4:1:4:12:**
```roc
Pair2(_, y) = Pair(0, 1)
```
^^^^^^^^^^^

The value being destructured has type:
        _[Pair(a, b), Pair2([], []), ..]
  where [
    a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),
    b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]),
  ]_

Missing patterns:
        _


**NON-EXHAUSTIVE DESTRUCTURE**
This destructuring pattern doesn't cover all possible cases:
**underscore_type_decl.md:5:1:5:12:**
```roc
Pair3(_, _) = Pair(0, 1)
```
^^^^^^^^^^^

The value being destructured has type:
        _[Pair(a, b), Pair3([], []), ..]
  where [
    a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),
    b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]),
  ]_

Missing patterns:
        _


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
