# META
~~~ini
description=Example of a nominal tag union with a payload
type=snippet
~~~
# SOURCE
~~~roc
Pair(a) := [Pair(a, a)]

pairU64 : Pair(U64)
pairU64 = Pair.Pair(1, 2)

pairStr : Pair(Str)
pairStr = Pair.Pair("hello", "world")

mkPair : a, a -> Pair(a)
mkPair = |x, y| Pair.Pair(x, y)

succeedPairSameType : Pair(U8)
succeedPairSameType = mkPair(1, 2)

failPairDiffTypes : Pair(U8)
failPairDiffTypes = mkPair("1", 2)

failPairDiffTypes2 : Pair(U64)
failPairDiffTypes2 = Pair.Pair(1, "str")

mkPairInvalid : a, b -> Pair(a)
mkPairInvalid = |x, y| Pair.Pair(x, y)
~~~
# EXPECTED
TYPE MISMATCH - annotations.md:16:28:16:28
INVALID NOMINAL TAG - annotations.md:19:22:19:41
INVALID NOMINAL TAG - annotations.md:22:24:22:39
# PROBLEMS
**TYPE MISMATCH**
The first and second arguments to `mkPair` must have compatible types, but they are incompatible in this call:
**annotations.md:16:28:**
```roc
failPairDiffTypes = mkPair("1", 2)
```
                           ^^^  ^

The first argument has the type:
    _Str_

But the second argument has the type:
    _Num(_size)_

`mkPair` needs these arguments to have compatible types.

**INVALID NOMINAL TAG**
I'm having trouble with this nominal tag:
**annotations.md:19:22:19:41:**
```roc
failPairDiffTypes2 = Pair.Pair(1, "str")
```
                     ^^^^^^^^^^^^^^^^^^^

The tag is:
    _Pair(Num(_size), Str)_

But the nominal type needs it to be:
    _Pair(Num(_size), Num(_size2))_

**INVALID NOMINAL TAG**
I'm having trouble with this nominal tag:
**annotations.md:22:24:22:39:**
```roc
mkPairInvalid = |x, y| Pair.Pair(x, y)
```
                       ^^^^^^^^^^^^^^^

The tag is:
    _Pair(a, b)_

But the nominal type needs it to be:
    _Pair(a, a)_

# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,CloseSquare,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,Comma,Int,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,Comma,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Pair")
				(args
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Pair"))
						(ty-var (raw "a"))
						(ty-var (raw "a"))))))
		(s-type-anno (name "pairU64")
			(ty-apply
				(ty (name "Pair"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "pairU64"))
			(e-apply
				(e-tag (raw "Pair.Pair"))
				(e-int (raw "1"))
				(e-int (raw "2"))))
		(s-type-anno (name "pairStr")
			(ty-apply
				(ty (name "Pair"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "pairStr"))
			(e-apply
				(e-tag (raw "Pair.Pair"))
				(e-string
					(e-string-part (raw "hello")))
				(e-string
					(e-string-part (raw "world")))))
		(s-type-anno (name "mkPair")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "a"))
				(ty-apply
					(ty (name "Pair"))
					(ty-var (raw "a")))))
		(s-decl
			(p-ident (raw "mkPair"))
			(e-lambda
				(args
					(p-ident (raw "x"))
					(p-ident (raw "y")))
				(e-apply
					(e-tag (raw "Pair.Pair"))
					(e-ident (raw "x"))
					(e-ident (raw "y")))))
		(s-type-anno (name "succeedPairSameType")
			(ty-apply
				(ty (name "Pair"))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "succeedPairSameType"))
			(e-apply
				(e-ident (raw "mkPair"))
				(e-int (raw "1"))
				(e-int (raw "2"))))
		(s-type-anno (name "failPairDiffTypes")
			(ty-apply
				(ty (name "Pair"))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "failPairDiffTypes"))
			(e-apply
				(e-ident (raw "mkPair"))
				(e-string
					(e-string-part (raw "1")))
				(e-int (raw "2"))))
		(s-type-anno (name "failPairDiffTypes2")
			(ty-apply
				(ty (name "Pair"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "failPairDiffTypes2"))
			(e-apply
				(e-tag (raw "Pair.Pair"))
				(e-int (raw "1"))
				(e-string
					(e-string-part (raw "str")))))
		(s-type-anno (name "mkPairInvalid")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b"))
				(ty-apply
					(ty (name "Pair"))
					(ty-var (raw "a")))))
		(s-decl
			(p-ident (raw "mkPairInvalid"))
			(e-lambda
				(args
					(p-ident (raw "x"))
					(p-ident (raw "y")))
				(e-apply
					(e-tag (raw "Pair.Pair"))
					(e-ident (raw "x"))
					(e-ident (raw "y")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "pairU64"))
		(e-nominal (nominal "Pair")
			(e-tag (name "Pair")
				(args
					(e-num (value "1"))
					(e-num (value "2")))))
		(annotation
			(ty-apply (name "Pair") (local)
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "pairStr"))
		(e-nominal (nominal "Pair")
			(e-tag (name "Pair")
				(args
					(e-string
						(e-literal (string "hello")))
					(e-string
						(e-literal (string "world"))))))
		(annotation
			(ty-apply (name "Pair") (local)
				(ty-lookup (name "Str") (external-module "Str")))))
	(d-let
		(p-assign (ident "mkPair"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "y")))
			(e-nominal (nominal "Pair")
				(e-tag (name "Pair")
					(args
						(e-lookup-local
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "y")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var-lookup (ty-rigid-var (name "a")))
				(ty-apply (name "Pair") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
	(d-let
		(p-assign (ident "succeedPairSameType"))
		(e-call
			(e-lookup-local
				(p-assign (ident "mkPair")))
			(e-num (value "1"))
			(e-num (value "2")))
		(annotation
			(ty-apply (name "Pair") (local)
				(ty-lookup (name "U8") (builtin)))))
	(d-let
		(p-assign (ident "failPairDiffTypes"))
		(e-call
			(e-lookup-local
				(p-assign (ident "mkPair")))
			(e-string
				(e-literal (string "1")))
			(e-num (value "2")))
		(annotation
			(ty-apply (name "Pair") (local)
				(ty-lookup (name "U8") (builtin)))))
	(d-let
		(p-assign (ident "failPairDiffTypes2"))
		(e-nominal (nominal "Pair")
			(e-tag (name "Pair")
				(args
					(e-num (value "1"))
					(e-string
						(e-literal (string "str"))))))
		(annotation
			(ty-apply (name "Pair") (local)
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "mkPairInvalid"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "y")))
			(e-nominal (nominal "Pair")
				(e-tag (name "Pair")
					(args
						(e-lookup-local
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "y")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))
				(ty-apply (name "Pair") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
	(s-nominal-decl
		(ty-header (name "Pair")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-tag-union
			(ty-tag-name (name "Pair")
				(ty-rigid-var-lookup (ty-rigid-var (name "a")))
				(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Pair(Num(Int(Unsigned64)))"))
		(patt (type "Pair(Error)"))
		(patt (type "a, a -> Pair(a)"))
		(patt (type "Pair(Num(Int(Unsigned8)))"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "a, b -> Error")))
	(type_decls
		(nominal (type "Pair(a)")
			(ty-header (name "Pair")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "Pair(Num(Int(Unsigned64)))"))
		(expr (type "Pair(Error)"))
		(expr (type "a, a -> Pair(a)"))
		(expr (type "Pair(Num(Int(Unsigned8)))"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "a, b -> Error"))))
~~~
