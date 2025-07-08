# META
~~~ini
description=Module exposes values that are not implemented
type=file
~~~
# SOURCE
~~~roc
module [foo, bar, MyType, OtherType]

# This module exposes foo, bar, MyType, and OtherType
# but only implements foo and MyType
# This should generate "exposed but not implemented" errors for bar and OtherType

foo = 42

MyType : [A, B, C]
~~~
# EXPECTED
NIL
# PROBLEMS
**EXPOSED BUT NOT DEFINED**
The module header says that ``bar`` is exposed, but it is not defined anywhere in this module.**exposed_not_impl.md:1:14:1:17:**
```roc
module [foo, bar, MyType, OtherType]
```
             ^^^
You can fix this by either defining ``bar`` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that ``OtherType`` is exposed, but it is not defined anywhere in this module.**exposed_not_impl.md:1:27:1:36:**
```roc
module [foo, bar, MyType, OtherType]
```
                          ^^^^^^^^^
You can fix this by either defining ``OtherType`` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),Comma(1:12-1:13),LowerIdent(1:14-1:17),Comma(1:17-1:18),UpperIdent(1:19-1:25),Comma(1:25-1:26),UpperIdent(1:27-1:36),CloseSquare(1:36-1:37),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:54),
Newline(4:2-4:37),
Newline(5:2-5:82),
Newline(1:1-1:1),
LowerIdent(7:1-7:4),OpAssign(7:5-7:6),Int(7:7-7:9),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(9:1-9:7),OpColon(9:8-9:9),OpenSquare(9:10-9:11),UpperIdent(9:11-9:12),Comma(9:12-9:13),UpperIdent(9:14-9:15),Comma(9:15-9:16),UpperIdent(9:17-9:18),CloseSquare(9:18-9:19),EndOfFile(9:19-9:19),
~~~
# PARSE
~~~clojure
(file @1.1-9.19
	(module @1.1-1.37
		(exposes @1.8-1.37
			(exposed-lower-ident (text "foo"))
			(exposed-lower-ident (text "bar"))
			(exposed-upper-ident (text "MyType"))
			(exposed-upper-ident (text "OtherType"))))
	(statements
		(s-decl @7.1-7.9
			(p-ident @7.1-7.4 (raw "foo"))
			(e-int @7.7-7.9 (raw "42")))
		(s-type-decl @9.1-9.19
			(header @9.1-9.7 (name "MyType")
				(args))
			(ty-tag-union @9.10-9.19
				(tags
					(ty @9.11-9.12 (name "A"))
					(ty @9.14-9.15 (name "B"))
					(ty @9.17-9.18 (name "C")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @7.1-7.4 (ident "foo"))
		(e-int @7.7-7.9 (value "42")))
	(s-alias-decl @9.1-9.19
		(ty-header @9.1-9.7 (name "MyType"))
		(ty-tag-union @9.10-9.19
			(ty @9.11-9.12 (name "A"))
			(ty @9.14-9.15 (name "B"))
			(ty @9.17-9.18 (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @7.1-7.4 (type "Num(*)")))
	(type_decls
		(alias @9.1-9.19 (type "MyType")
			(ty-header @9.1-9.7 (name "MyType"))))
	(expressions
		(expr @7.7-7.9 (type "Num(*)"))))
~~~
