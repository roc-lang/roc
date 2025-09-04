# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]s:b->c where module(a).t:c,u:o...
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_057.md:1:39:1:42
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_057.md:1:39:1:42:**
```roc
module[]s:b->c where module(a).t:c,u:o...
```
                                      ^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),LowerIdent(1:9-1:10),OpColon(1:10-1:11),LowerIdent(1:11-1:12),OpArrow(1:12-1:14),LowerIdent(1:14-1:15),KwWhere(1:16-1:21),KwModule(1:22-1:28),NoSpaceOpenRound(1:28-1:29),LowerIdent(1:29-1:30),CloseRound(1:30-1:31),NoSpaceDotLowerIdent(1:31-1:33),OpColon(1:33-1:34),LowerIdent(1:34-1:35),Comma(1:35-1:36),LowerIdent(1:36-1:37),OpColon(1:37-1:38),LowerIdent(1:38-1:39),TripleDot(1:39-1:42),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.42
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(s-type-anno @1.9-1.36 (name "s")
			(ty-fn @1.11-1.15
				(ty-var @1.11-1.12 (raw "b"))
				(ty-var @1.14-1.15 (raw "c")))
			(where
				(method @1.22-1.35 (module-of "a") (name "t")
					(args)
					(ty-var @1.34-1.35 (raw "c")))))
		(s-type-anno @1.36-1.39 (name "u")
			(ty-var @1.38-1.39 (raw "o")))
		(s-malformed @1.39-1.42 (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module []
s : b -> c where module(a).t : c
u : o
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-type-anno @1.9-1.36 (name "s")
		(ty-fn @1.11-1.15 (effectful false)
			(ty-var @1.11-1.12 (name "b"))
			(ty-var @1.14-1.15 (name "c")))
		(where
			(method @1.22-1.35 (module-of "a") (ident "t")
				(args)
				(ty-var @1.34-1.35 (name "c")))))
	(ext-decl @1.22-1.35 (ident "module(a).t") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
