# META
~~~ini
description=Module exposes values that are not implemented
type=file
~~~
# SOURCE
~~~roc
module [foo, bar, MyType, OtherType, foo, MyType]

# This module exposes foo, bar, MyType, and OtherType
# but only implements foo and MyType
# This should generate "exposed but not implemented" errors for bar and OtherType
# Also tests redundant exposed entries for foo and MyType

foo = 42

MyType : [A, B, C]
~~~
# EXPECTED
REDUNDANT EXPOSED - exposed_not_impl.md:1:38:1:41
REDUNDANT EXPOSED - exposed_not_impl.md:1:43:1:49
EXPOSED BUT NOT DEFINED - exposed_not_impl.md:1:14:1:17
EXPOSED BUT NOT DEFINED - exposed_not_impl.md:1:27:1:36
# PROBLEMS
**REDUNDANT EXPOSED**
The identifier `foo` is exposed multiple times in the module header.

**exposed_not_impl.md:1:38:1:41:**
```roc
module [foo, bar, MyType, OtherType, foo, MyType]
```
                                     ^^^
You can remove the duplicate entry to fix this warning.

**REDUNDANT EXPOSED**
The identifier `myType` is exposed multiple times in the module header.

**exposed_not_impl.md:1:43:1:49:**
```roc
module [foo, bar, MyType, OtherType, foo, MyType]
```
                                          ^^^^^^
You can remove the duplicate entry to fix this warning.

**EXPOSED BUT NOT DEFINED**
The module header says that `bar` is exposed, but it is not defined anywhere in this module.

**exposed_not_impl.md:1:14:1:17:**
```roc
module [foo, bar, MyType, OtherType, foo, MyType]
```
             ^^^
You can fix this by either defining `bar` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that `otherType` is exposed, but it is not defined anywhere in this module.

**exposed_not_impl.md:1:27:1:36:**
```roc
module [foo, bar, MyType, OtherType, foo, MyType]
```
                          ^^^^^^^^^
You can fix this by either defining `otherType` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),Comma(1:12-1:13),LowerIdent(1:14-1:17),Comma(1:17-1:18),UpperIdent(1:19-1:25),Comma(1:25-1:26),UpperIdent(1:27-1:36),Comma(1:36-1:37),LowerIdent(1:38-1:41),Comma(1:41-1:42),UpperIdent(1:43-1:49),CloseSquare(1:49-1:50),
LowerIdent(8:1-8:4),OpAssign(8:5-8:6),Int(8:7-8:9),
UpperIdent(10:1-10:7),OpColon(10:8-10:9),OpenSquare(10:10-10:11),UpperIdent(10:11-10:12),Comma(10:12-10:13),UpperIdent(10:14-10:15),Comma(10:15-10:16),UpperIdent(10:17-10:18),CloseSquare(10:18-10:19),EndOfFile(10:19-10:19),
~~~
# PARSE
~~~clojure
(file @1.1-10.19
	(module @1.1-1.50
		(exposes @1.8-1.50
			(exposed-lower-ident @1.9-1.12
				(text "foo"))
			(exposed-lower-ident @1.14-1.17
				(text "bar"))
			(exposed-upper-ident @1.19-1.25 (text "MyType"))
			(exposed-upper-ident @1.27-1.36 (text "OtherType"))
			(exposed-lower-ident @1.38-1.41
				(text "foo"))
			(exposed-upper-ident @1.43-1.49 (text "MyType"))))
	(statements
		(s-decl @8.1-8.9
			(p-ident @8.1-8.4 (raw "foo"))
			(e-int @8.7-8.9 (raw "42")))
		(s-type-decl @10.1-10.19
			(header @10.1-10.7 (name "MyType")
				(args))
			(ty-tag-union @10.10-10.19
				(tags
					(ty @10.11-10.12 (name "A"))
					(ty @10.14-10.15 (name "B"))
					(ty @10.17-10.18 (name "C")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @8.1-8.4 (ident "foo"))
		(e-int @8.7-8.9 (value "42")))
	(s-alias-decl @10.1-10.19
		(ty-header @10.1-10.7 (name "MyType"))
		(ty-tag-union @10.10-10.19
			(ty @10.11-10.12 (name "A"))
			(ty @10.14-10.15 (name "B"))
			(ty @10.17-10.18 (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @8.1-8.4 (type "Num(_size)")))
	(type_decls
		(alias @10.1-10.19 (type "MyType")
			(ty-header @10.1-10.7 (name "MyType"))))
	(expressions
		(expr @8.7-8.9 (type "Num(_size)"))))
~~~
