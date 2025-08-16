# META
~~~ini
description=Multiple where constraints on different type variables
type=file
~~~
# SOURCE
~~~roc
module [process]

process : a, b -> c where module(a).convert : a -> c, module(b).transform : b -> c
process = |_, _| ...
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **module** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**where_clauses_multi_type_vars.md:3:55:3:61:**
```roc
process : a, b -> c where module(a).convert : a -> c, module(b).transform : b -> c
```
                                                      ^^^^^^


**PARSE ERROR**
A parsing error occurred: `expected_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_clauses_multi_type_vars.md:3:52:3:53:**
```roc
process : a, b -> c where module(a).convert : a -> c, module(b).transform : b -> c
```
                                                   ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_clauses_multi_type_vars.md:3:62:3:63:**
```roc
process : a, b -> c where module(a).convert : a -> c, module(b).transform : b -> c
```
                                                             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_clauses_multi_type_vars.md:3:63:3:64:**
```roc
process : a, b -> c where module(a).convert : a -> c, module(b).transform : b -> c
```
                                                              ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_clauses_multi_type_vars.md:3:64:3:74:**
```roc
process : a, b -> c where module(a).convert : a -> c, module(b).transform : b -> c
```
                                                               ^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_clauses_multi_type_vars.md:3:75:3:76:**
```roc
process : a, b -> c where module(a).convert : a -> c, module(b).transform : b -> c
```
                                                                          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_clauses_multi_type_vars.md:3:77:3:78:**
```roc
process : a, b -> c where module(a).convert : a -> c, module(b).transform : b -> c
```
                                                                            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_clauses_multi_type_vars.md:3:79:3:81:**
```roc
process : a, b -> c where module(a).convert : a -> c, module(b).transform : b -> c
```
                                                                              ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_clauses_multi_type_vars.md:3:82:3:83:**
```roc
process : a, b -> c where module(a).convert : a -> c, module(b).transform : b -> c
```
                                                                                 ^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**where_clauses_multi_type_vars.md:3:52:3:62:**
```roc
process : a, b -> c where module(a).convert : a -> c, module(b).transform : b -> c
```
                                                   ^^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:16),CloseSquare(1:16-1:17),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),LowerIdent(3:11-3:12),Comma(3:12-3:13),LowerIdent(3:14-3:15),OpArrow(3:16-3:18),LowerIdent(3:19-3:20),KwWhere(3:21-3:26),KwModule(3:27-3:33),NoSpaceOpenRound(3:33-3:34),LowerIdent(3:34-3:35),CloseRound(3:35-3:36),NoSpaceDotLowerIdent(3:36-3:44),OpColon(3:45-3:46),LowerIdent(3:47-3:48),OpArrow(3:49-3:51),LowerIdent(3:52-3:53),Comma(3:53-3:54),KwModule(3:55-3:61),NoSpaceOpenRound(3:61-3:62),LowerIdent(3:62-3:63),CloseRound(3:63-3:64),NoSpaceDotLowerIdent(3:64-3:74),OpColon(3:75-3:76),LowerIdent(3:77-3:78),OpArrow(3:79-3:81),LowerIdent(3:82-3:83),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),OpBar(4:11-4:12),Underscore(4:12-4:13),Comma(4:13-4:14),Underscore(4:15-4:16),OpBar(4:16-4:17),TripleDot(4:18-4:21),EndOfFile(4:21-4:21),
~~~
# PARSE
~~~clojure
(file @1.1-4.21
	(module @1.1-1.17
		(exposes @1.8-1.17
			(exposed-lower-ident @1.9-1.16
				(text "process"))))
	(statements
		(s-type-anno @3.1-3.62 (name "process")
			(ty-fn @3.11-3.20
				(ty-var @3.11-3.12 (raw "a"))
				(ty-var @3.14-3.15 (raw "b"))
				(ty-var @3.19-3.20 (raw "c")))
			(where
				(method @3.27-3.62 (module-of "a") (name "convert")
					(args
						(ty-var @3.47-3.48 (raw "a")))
					(ty-malformed @3.52-3.62 (tag "expected_arrow")))))
		(s-malformed @3.62-3.63 (tag "statement_unexpected_token"))
		(s-malformed @3.63-3.64 (tag "statement_unexpected_token"))
		(s-malformed @3.64-3.74 (tag "statement_unexpected_token"))
		(s-malformed @3.75-3.76 (tag "statement_unexpected_token"))
		(s-malformed @3.77-3.78 (tag "statement_unexpected_token"))
		(s-malformed @3.79-3.81 (tag "statement_unexpected_token"))
		(s-malformed @3.82-3.83 (tag "statement_unexpected_token"))
		(s-decl @4.1-4.21
			(p-ident @4.1-4.8 (raw "process"))
			(e-lambda @4.11-4.21
				(args
					(p-underscore)
					(p-underscore))
				(e-ellipsis)))))
~~~
# FORMATTED
~~~roc
module [process]

process : a, b -> c where module(a).convert : a -> 

process = |_, _| ...
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.8 (ident "process"))
		(e-lambda @4.11-4.21
			(args
				(p-underscore @4.12-4.13)
				(p-underscore @4.15-4.16))
			(e-not-implemented @1.1-1.1)))
	(s-type-anno @3.1-3.62 (name "process")
		(ty-fn @3.11-3.20 (effectful false)
			(ty-var @3.11-3.12 (name "a"))
			(ty-var @3.14-3.15 (name "b"))
			(ty-var @3.19-3.20 (name "c")))
		(where
			(method @3.27-3.62 (module-of "a") (ident "convert")
				(args
					(ty-var @3.47-3.48 (name "a")))
				(ty-malformed @3.52-3.62))))
	(ext-decl @3.27-3.62 (ident "module(a).convert") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.8 (type "_arg, _arg2 -> _ret")))
	(expressions
		(expr @4.11-4.21 (type "_arg, _arg2 -> _ret"))))
~~~
