# META
~~~ini
description=underscore_in_assignment_pattern
type=file
~~~
# SOURCE
~~~roc
module []

import Module exposing [Pair]

Pair1(x, _) = Pair(0, 1)
Pair2(_, y) = Pair(0, 1)
Pair3(_, _) = Pair(0, 1)
~~~
# EXPECTED
MODULE NOT FOUND - underscore_type_decl.md:3:1:3:30
# PROBLEMS
**MODULE NOT FOUND**
The module `Module` was not found in this Roc project.

You're attempting to use this module here:
**underscore_type_decl.md:3:1:3:30:**
```roc
import Module exposing [Pair]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
KwImport(3:1-3:7),UpperIdent(3:8-3:14),KwExposing(3:15-3:23),OpenSquare(3:24-3:25),UpperIdent(3:25-3:29),CloseSquare(3:29-3:30),
UpperIdent(5:1-5:6),NoSpaceOpenRound(5:6-5:7),LowerIdent(5:7-5:8),Comma(5:8-5:9),Underscore(5:10-5:11),CloseRound(5:11-5:12),OpAssign(5:13-5:14),UpperIdent(5:15-5:19),NoSpaceOpenRound(5:19-5:20),Int(5:20-5:21),Comma(5:21-5:22),Int(5:23-5:24),CloseRound(5:24-5:25),
UpperIdent(6:1-6:6),NoSpaceOpenRound(6:6-6:7),Underscore(6:7-6:8),Comma(6:8-6:9),LowerIdent(6:10-6:11),CloseRound(6:11-6:12),OpAssign(6:13-6:14),UpperIdent(6:15-6:19),NoSpaceOpenRound(6:19-6:20),Int(6:20-6:21),Comma(6:21-6:22),Int(6:23-6:24),CloseRound(6:24-6:25),
UpperIdent(7:1-7:6),NoSpaceOpenRound(7:6-7:7),Underscore(7:7-7:8),Comma(7:8-7:9),Underscore(7:10-7:11),CloseRound(7:11-7:12),OpAssign(7:13-7:14),UpperIdent(7:15-7:19),NoSpaceOpenRound(7:19-7:20),Int(7:20-7:21),Comma(7:21-7:22),Int(7:23-7:24),CloseRound(7:24-7:25),EndOfFile(7:25-7:25),
~~~
# PARSE
~~~clojure
(file @1.1-7.25
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-import @3.1-3.30 (raw "Module")
			(exposing
				(exposed-upper-ident @3.25-3.29 (text "Pair"))))
		(s-decl @5.1-5.25
			(p-tag @5.1-5.12 (raw "Pair1")
				(p-ident @5.7-5.8 (raw "x"))
				(p-underscore))
			(e-apply @5.15-5.25
				(e-tag @5.15-5.19 (raw "Pair"))
				(e-int @5.20-5.21 (raw "0"))
				(e-int @5.23-5.24 (raw "1"))))
		(s-decl @6.1-6.25
			(p-tag @6.1-6.12 (raw "Pair2")
				(p-underscore)
				(p-ident @6.10-6.11 (raw "y")))
			(e-apply @6.15-6.25
				(e-tag @6.15-6.19 (raw "Pair"))
				(e-int @6.20-6.21 (raw "0"))
				(e-int @6.23-6.24 (raw "1"))))
		(s-decl @7.1-7.25
			(p-tag @7.1-7.12 (raw "Pair3")
				(p-underscore)
				(p-underscore))
			(e-apply @7.15-7.25
				(e-tag @7.15-7.19 (raw "Pair"))
				(e-int @7.20-7.21 (raw "0"))
				(e-int @7.23-7.24 (raw "1"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-applied-tag @5.1-5.12)
		(e-tag @5.15-5.25 (name "Pair")
			(args
				(e-int @5.20-5.21 (value "0"))
				(e-int @5.23-5.24 (value "1")))))
	(d-let
		(p-applied-tag @6.1-6.12)
		(e-tag @6.15-6.25 (name "Pair")
			(args
				(e-int @6.20-6.21 (value "0"))
				(e-int @6.23-6.24 (value "1")))))
	(d-let
		(p-applied-tag @7.1-7.12)
		(e-tag @7.15-7.25 (name "Pair")
			(args
				(e-int @7.20-7.21 (value "0"))
				(e-int @7.23-7.24 (value "1")))))
	(s-import @3.1-3.30 (module "Module")
		(exposes
			(exposed (name "Pair") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions
		(expr @5.15-5.25 (type "[Pair1(_a, _b), Pair(Num(_size), Num(_size2))]_others"))
		(expr @6.15-6.25 (type "[Pair2(_a, _b), Pair(Num(_size), Num(_size2))]_others"))
		(expr @7.15-7.25 (type "[Pair3(_a, _b), Pair(Num(_size), Num(_size2))]_others"))))
~~~
