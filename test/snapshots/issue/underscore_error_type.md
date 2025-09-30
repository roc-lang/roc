# META
~~~ini
description=Type declarations with underscores should become error types that fail unification
type=file:BadType.roc
~~~
# SOURCE
~~~roc
BadType := _

foo : BadType
foo = 42

BadList := List(_)

bar : BadList
bar = [1, 2, 3]

BadRecord := { field: _, other: U32 }

baz : BadRecord
baz = { field: "hi", other: 5 }

BadFunction := _ -> _

qux : BadFunction
qux = |x| x

BadTuple := (_, U32)

quux : BadTuple
quux = ("hello", 42)
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:6:17:6:17
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:21:14:21:14
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_error_type.md:1:1:1:1:**
```roc
BadType := _
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_error_type.md:6:17:6:17:**
```roc
BadList := List(_)
```
                ^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_error_type.md:1:1:1:1:**
```roc
BadType := _
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_error_type.md:1:1:1:1:**
```roc
BadType := _
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_error_type.md:1:1:1:1:**
```roc
BadType := _
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_error_type.md:21:14:21:14:**
```roc
BadTuple := (_, U32)
```
             ^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

# TOKENS
~~~zig
UpperIdent(1:1-1:8),OpColonEqual(1:9-1:11),Underscore(1:12-1:13),
LowerIdent(3:1-3:4),OpColon(3:5-3:6),UpperIdent(3:7-3:14),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),Int(4:7-4:9),
UpperIdent(6:1-6:8),OpColonEqual(6:9-6:11),UpperIdent(6:12-6:16),NoSpaceOpenRound(6:16-6:17),Underscore(6:17-6:18),CloseRound(6:18-6:19),
LowerIdent(8:1-8:4),OpColon(8:5-8:6),UpperIdent(8:7-8:14),
LowerIdent(9:1-9:4),OpAssign(9:5-9:6),OpenSquare(9:7-9:8),Int(9:8-9:9),Comma(9:9-9:10),Int(9:11-9:12),Comma(9:12-9:13),Int(9:14-9:15),CloseSquare(9:15-9:16),
UpperIdent(11:1-11:10),OpColonEqual(11:11-11:13),OpenCurly(11:14-11:15),LowerIdent(11:16-11:21),OpColon(11:21-11:22),Underscore(11:23-11:24),Comma(11:24-11:25),LowerIdent(11:26-11:31),OpColon(11:31-11:32),UpperIdent(11:33-11:36),CloseCurly(11:37-11:38),
LowerIdent(13:1-13:4),OpColon(13:5-13:6),UpperIdent(13:7-13:16),
LowerIdent(14:1-14:4),OpAssign(14:5-14:6),OpenCurly(14:7-14:8),LowerIdent(14:9-14:14),OpColon(14:14-14:15),StringStart(14:16-14:17),StringPart(14:17-14:19),StringEnd(14:19-14:20),Comma(14:20-14:21),LowerIdent(14:22-14:27),OpColon(14:27-14:28),Int(14:29-14:30),CloseCurly(14:31-14:32),
UpperIdent(16:1-16:12),OpColonEqual(16:13-16:15),Underscore(16:16-16:17),OpArrow(16:18-16:20),Underscore(16:21-16:22),
LowerIdent(18:1-18:4),OpColon(18:5-18:6),UpperIdent(18:7-18:18),
LowerIdent(19:1-19:4),OpAssign(19:5-19:6),OpBar(19:7-19:8),LowerIdent(19:8-19:9),OpBar(19:9-19:10),LowerIdent(19:11-19:12),
UpperIdent(21:1-21:9),OpColonEqual(21:10-21:12),OpenRound(21:13-21:14),Underscore(21:14-21:15),Comma(21:15-21:16),UpperIdent(21:17-21:20),CloseRound(21:20-21:21),
LowerIdent(23:1-23:5),OpColon(23:6-23:7),UpperIdent(23:8-23:16),
LowerIdent(24:1-24:5),OpAssign(24:6-24:7),OpenRound(24:8-24:9),StringStart(24:9-24:10),StringPart(24:10-24:15),StringEnd(24:15-24:16),Comma(24:16-24:17),Int(24:18-24:20),CloseRound(24:20-24:21),
EndOfFile(25:1-25:1),
~~~
# PARSE
~~~clojure
(file @1.1-24.21
	(type-module @1.1-1.8)
	(statements
		(s-type-decl @1.1-1.13
			(header @1.1-1.8 (name "BadType")
				(args))
			(_))
		(s-type-anno @3.1-3.14 (name "foo")
			(ty @3.7-3.14 (name "BadType")))
		(s-decl @4.1-4.9
			(p-ident @4.1-4.4 (raw "foo"))
			(e-int @4.7-4.9 (raw "42")))
		(s-type-decl @6.1-6.19
			(header @6.1-6.8 (name "BadList")
				(args))
			(ty-apply @6.12-6.19
				(ty @6.12-6.16 (name "List"))
				(_)))
		(s-type-anno @8.1-8.14 (name "bar")
			(ty @8.7-8.14 (name "BadList")))
		(s-decl @9.1-9.16
			(p-ident @9.1-9.4 (raw "bar"))
			(e-list @9.7-9.16
				(e-int @9.8-9.9 (raw "1"))
				(e-int @9.11-9.12 (raw "2"))
				(e-int @9.14-9.15 (raw "3"))))
		(s-type-decl @11.1-11.38
			(header @11.1-11.10 (name "BadRecord")
				(args))
			(ty-record @11.14-11.38
				(anno-record-field @11.16-11.24 (name "field")
					(_))
				(anno-record-field @11.26-11.36 (name "other")
					(ty @11.33-11.36 (name "U32")))))
		(s-type-anno @13.1-13.16 (name "baz")
			(ty @13.7-13.16 (name "BadRecord")))
		(s-decl @14.1-14.32
			(p-ident @14.1-14.4 (raw "baz"))
			(e-record @14.7-14.32
				(field (field "field")
					(e-string @14.16-14.20
						(e-string-part @14.17-14.19 (raw "hi"))))
				(field (field "other")
					(e-int @14.29-14.30 (raw "5")))))
		(s-type-decl @16.1-16.22
			(header @16.1-16.12 (name "BadFunction")
				(args))
			(ty-fn @16.16-16.22
				(_)
				(_)))
		(s-type-anno @18.1-18.18 (name "qux")
			(ty @18.7-18.18 (name "BadFunction")))
		(s-decl @19.1-19.12
			(p-ident @19.1-19.4 (raw "qux"))
			(e-lambda @19.7-19.12
				(args
					(p-ident @19.8-19.9 (raw "x")))
				(e-ident @19.11-19.12 (raw "x"))))
		(s-type-decl @21.1-21.21
			(header @21.1-21.9 (name "BadTuple")
				(args))
			(ty-tuple @21.13-21.21
				(_)
				(ty @21.17-21.20 (name "U32"))))
		(s-type-anno @23.1-23.16 (name "quux")
			(ty @23.8-23.16 (name "BadTuple")))
		(s-decl @24.1-24.21
			(p-ident @24.1-24.5 (raw "quux"))
			(e-tuple @24.8-24.21
				(e-string @24.9-24.16
					(e-string-part @24.10-24.15 (raw "hello")))
				(e-int @24.18-24.20 (raw "42"))))))
~~~
# FORMATTED
~~~roc
BadType := _

foo : BadType
foo = 42

BadList := List(_)

bar : BadList
bar = [1, 2, 3]

BadRecord := { field : _, other : U32 }

baz : BadRecord
baz = { field: "hi", other: 5 }

BadFunction := _ -> _

qux : BadFunction
qux = |x| x

BadTuple := (_, U32)

quux : BadTuple
quux = ("hello", 42)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.4 (ident "foo"))
		(e-int @4.7-4.9 (value "42"))
		(annotation @4.1-4.4
			(declared-type
				(ty @3.7-3.14 (name "BadType")))))
	(d-let
		(p-assign @9.1-9.4 (ident "bar"))
		(e-list @9.7-9.16
			(elems
				(e-int @9.8-9.9 (value "1"))
				(e-int @9.11-9.12 (value "2"))
				(e-int @9.14-9.15 (value "3"))))
		(annotation @9.1-9.4
			(declared-type
				(ty @8.7-8.14 (name "BadList")))))
	(d-let
		(p-assign @14.1-14.4 (ident "baz"))
		(e-record @14.7-14.32
			(fields
				(field (name "field")
					(e-string @14.16-14.20
						(e-literal @14.17-14.19 (string "hi"))))
				(field (name "other")
					(e-int @14.29-14.30 (value "5")))))
		(annotation @14.1-14.4
			(declared-type
				(ty @13.7-13.16 (name "BadRecord")))))
	(d-let
		(p-assign @19.1-19.4 (ident "qux"))
		(e-lambda @19.7-19.12
			(args
				(p-assign @19.8-19.9 (ident "x")))
			(e-lookup-local @19.11-19.12
				(p-assign @19.8-19.9 (ident "x"))))
		(annotation @19.1-19.4
			(declared-type
				(ty @18.7-18.18 (name "BadFunction")))))
	(d-let
		(p-assign @24.1-24.5 (ident "quux"))
		(e-tuple @24.8-24.21
			(elems
				(e-string @24.9-24.16
					(e-literal @24.10-24.15 (string "hello")))
				(e-int @24.18-24.20 (value "42"))))
		(annotation @24.1-24.5
			(declared-type
				(ty @23.8-23.16 (name "BadTuple")))))
	(s-nominal-decl @1.1-1.13
		(ty-header @1.1-1.8 (name "BadType"))
		(ty-underscore @1.1-1.1))
	(s-nominal-decl @6.1-6.19
		(ty-header @6.1-6.8 (name "BadList"))
		(ty-apply @6.12-6.19 (symbol "List")
			(ty-underscore @6.17-6.17)))
	(s-nominal-decl @11.1-11.38
		(ty-header @11.1-11.10 (name "BadRecord"))
		(ty-record @11.14-11.38
			(field (field "field")
				(ty-underscore @1.1-1.1))
			(field (field "other")
				(ty @11.33-11.36 (name "U32")))))
	(s-nominal-decl @16.1-16.22
		(ty-header @16.1-16.12 (name "BadFunction"))
		(ty-fn @16.16-16.22 (effectful false)
			(ty-underscore @1.1-1.1)
			(ty-underscore @1.1-1.1)))
	(s-nominal-decl @21.1-21.21
		(ty-header @21.1-21.9 (name "BadTuple"))
		(ty-tuple @21.13-21.21
			(ty-underscore @21.14-21.14)
			(ty @21.17-21.20 (name "U32")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.4 (type "Error"))
		(patt @9.1-9.4 (type "Error"))
		(patt @14.1-14.4 (type "Error"))
		(patt @19.1-19.4 (type "Error"))
		(patt @24.1-24.5 (type "Error")))
	(type_decls
		(nominal @1.1-1.13 (type "Error")
			(ty-header @1.1-1.8 (name "BadType")))
		(nominal @6.1-6.19 (type "Error")
			(ty-header @6.1-6.8 (name "BadList")))
		(nominal @11.1-11.38 (type "Error")
			(ty-header @11.1-11.10 (name "BadRecord")))
		(nominal @16.1-16.22 (type "Error")
			(ty-header @16.1-16.12 (name "BadFunction")))
		(nominal @21.1-21.21 (type "Error")
			(ty-header @21.1-21.9 (name "BadTuple"))))
	(expressions
		(expr @4.7-4.9 (type "Error"))
		(expr @9.7-9.16 (type "Error"))
		(expr @14.7-14.32 (type "Error"))
		(expr @19.7-19.12 (type "Error"))
		(expr @24.8-24.21 (type "Error"))))
~~~
