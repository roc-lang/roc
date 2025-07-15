# META
~~~ini
description=Type declarations with underscores should become error types that fail unification
type=file
~~~
# SOURCE
~~~roc
module []

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
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:8:17:8:17
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:23:14:23:14
TYPE MISMATCH - underscore_error_type.md:15:7:15:16
TYPE MISMATCH - underscore_error_type.md:20:7:20:18
TYPE MISMATCH - underscore_error_type.md:25:8:25:16
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_error_type.md:1:1:1:1:**
```roc

```


Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_error_type.md:8:17:8:17:**
```roc
BadList := List(_)
```
                

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_error_type.md:1:1:1:1:**
```roc

```


Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_error_type.md:1:1:1:1:**
```roc

```


Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_error_type.md:1:1:1:1:**
```roc

```


Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_error_type.md:23:14:23:14:**
```roc
BadTuple := (_, U32)
```
             

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**TYPE MISMATCH**
This expression is used in an unexpected way:
**underscore_error_type.md:15:7:15:16:**
```roc
baz : BadRecord
```
      ^^^^^^^^^

It is of type:
    _BadRecord_

But you are trying to use it as:
    _{ field: Str, other: Num(_size) }_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**underscore_error_type.md:20:7:20:18:**
```roc
qux : BadFunction
```
      ^^^^^^^^^^^

It is of type:
    _BadFunction_

But you are trying to use it as:
    __arg -> _ret_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**underscore_error_type.md:25:8:25:16:**
```roc
quux : BadTuple
```
       ^^^^^^^^

It is of type:
    _BadTuple_

But you are trying to use it as:
    _(Str, Num(_size))_

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
UpperIdent(3:1-3:8),OpColonEqual(3:9-3:11),Underscore(3:12-3:13),
LowerIdent(5:1-5:4),OpColon(5:5-5:6),UpperIdent(5:7-5:14),
LowerIdent(6:1-6:4),OpAssign(6:5-6:6),Int(6:7-6:9),
UpperIdent(8:1-8:8),OpColonEqual(8:9-8:11),UpperIdent(8:12-8:16),NoSpaceOpenRound(8:16-8:17),Underscore(8:17-8:18),CloseRound(8:18-8:19),
LowerIdent(10:1-10:4),OpColon(10:5-10:6),UpperIdent(10:7-10:14),
LowerIdent(11:1-11:4),OpAssign(11:5-11:6),OpenSquare(11:7-11:8),Int(11:8-11:9),Comma(11:9-11:10),Int(11:11-11:12),Comma(11:12-11:13),Int(11:14-11:15),CloseSquare(11:15-11:16),
UpperIdent(13:1-13:10),OpColonEqual(13:11-13:13),OpenCurly(13:14-13:15),LowerIdent(13:16-13:21),OpColon(13:21-13:22),Underscore(13:23-13:24),Comma(13:24-13:25),LowerIdent(13:26-13:31),OpColon(13:31-13:32),UpperIdent(13:33-13:36),CloseCurly(13:37-13:38),
LowerIdent(15:1-15:4),OpColon(15:5-15:6),UpperIdent(15:7-15:16),
LowerIdent(16:1-16:4),OpAssign(16:5-16:6),OpenCurly(16:7-16:8),LowerIdent(16:9-16:14),OpColon(16:14-16:15),StringStart(16:16-16:17),StringPart(16:17-16:19),StringEnd(16:19-16:20),Comma(16:20-16:21),LowerIdent(16:22-16:27),OpColon(16:27-16:28),Int(16:29-16:30),CloseCurly(16:31-16:32),
UpperIdent(18:1-18:12),OpColonEqual(18:13-18:15),Underscore(18:16-18:17),OpArrow(18:18-18:20),Underscore(18:21-18:22),
LowerIdent(20:1-20:4),OpColon(20:5-20:6),UpperIdent(20:7-20:18),
LowerIdent(21:1-21:4),OpAssign(21:5-21:6),OpBar(21:7-21:8),LowerIdent(21:8-21:9),OpBar(21:9-21:10),LowerIdent(21:11-21:12),
UpperIdent(23:1-23:9),OpColonEqual(23:10-23:12),OpenRound(23:13-23:14),Underscore(23:14-23:15),Comma(23:15-23:16),UpperIdent(23:17-23:20),CloseRound(23:20-23:21),
LowerIdent(25:1-25:5),OpColon(25:6-25:7),UpperIdent(25:8-25:16),
LowerIdent(26:1-26:5),OpAssign(26:6-26:7),OpenRound(26:8-26:9),StringStart(26:9-26:10),StringPart(26:10-26:15),StringEnd(26:15-26:16),Comma(26:16-26:17),Int(26:18-26:20),CloseRound(26:20-26:21),EndOfFile(26:21-26:21),
~~~
# PARSE
~~~clojure
(file @1.1-26.21
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-decl @3.1-3.13
			(header @3.1-3.8 (name "BadType")
				(args))
			(_))
		(s-type-anno @5.1-5.14 (name "foo")
			(ty @5.7-5.14 (name "BadType")))
		(s-decl @6.1-6.9
			(p-ident @6.1-6.4 (raw "foo"))
			(e-int @6.7-6.9 (raw "42")))
		(s-type-decl @8.1-8.19
			(header @8.1-8.8 (name "BadList")
				(args))
			(ty-apply @8.12-8.19
				(ty @8.12-8.16 (name "List"))
				(_)))
		(s-type-anno @10.1-10.14 (name "bar")
			(ty @10.7-10.14 (name "BadList")))
		(s-decl @11.1-11.16
			(p-ident @11.1-11.4 (raw "bar"))
			(e-list @11.7-11.16
				(e-int @11.8-11.9 (raw "1"))
				(e-int @11.11-11.12 (raw "2"))
				(e-int @11.14-11.15 (raw "3"))))
		(s-type-decl @13.1-13.38
			(header @13.1-13.10 (name "BadRecord")
				(args))
			(ty-record @13.14-13.38
				(anno-record-field @13.16-13.24 (name "field")
					(_))
				(anno-record-field @13.26-13.36 (name "other")
					(ty @13.33-13.36 (name "U32")))))
		(s-type-anno @15.1-15.16 (name "baz")
			(ty @15.7-15.16 (name "BadRecord")))
		(s-decl @16.1-16.32
			(p-ident @16.1-16.4 (raw "baz"))
			(e-record @16.7-16.32
				(field (field "field")
					(e-string @16.16-16.20
						(e-string-part @16.17-16.19 (raw "hi"))))
				(field (field "other")
					(e-int @16.29-16.30 (raw "5")))))
		(s-type-decl @18.1-18.22
			(header @18.1-18.12 (name "BadFunction")
				(args))
			(ty-fn @18.16-18.22
				(_)
				(_)))
		(s-type-anno @20.1-20.18 (name "qux")
			(ty @20.7-20.18 (name "BadFunction")))
		(s-decl @21.1-21.12
			(p-ident @21.1-21.4 (raw "qux"))
			(e-lambda @21.7-21.12
				(args
					(p-ident @21.8-21.9 (raw "x")))
				(e-ident @21.11-21.12 (raw "x"))))
		(s-type-decl @23.1-23.21
			(header @23.1-23.9 (name "BadTuple")
				(args))
			(ty-tuple @23.13-23.21
				(_)
				(ty @23.17-23.20 (name "U32"))))
		(s-type-anno @25.1-25.16 (name "quux")
			(ty @25.8-25.16 (name "BadTuple")))
		(s-decl @26.1-26.21
			(p-ident @26.1-26.5 (raw "quux"))
			(e-tuple @26.8-26.21
				(e-string @26.9-26.16
					(e-string-part @26.10-26.15 (raw "hello")))
				(e-int @26.18-26.20 (raw "42"))))))
~~~
# FORMATTED
~~~roc
module []

BadType := _

foo : BadType
foo = 42

BadList := List(_)

bar : BadList
bar = [1, 2, 3]

BadRecord := { field : _, other : U32 }

baz : BadRecord
baz = {field: "hi", other: 5}

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
		(p-assign @6.1-6.4 (ident "foo"))
		(e-int @6.7-6.9 (value "42"))
		(annotation @6.1-6.4
			(declared-type
				(ty @5.7-5.14 (name "BadType")))))
	(d-let
		(p-assign @11.1-11.4 (ident "bar"))
		(e-list @11.7-11.16
			(elems
				(e-int @11.8-11.9 (value "1"))
				(e-int @11.11-11.12 (value "2"))
				(e-int @11.14-11.15 (value "3"))))
		(annotation @11.1-11.4
			(declared-type
				(ty @10.7-10.14 (name "BadList")))))
	(d-let
		(p-assign @16.1-16.4 (ident "baz"))
		(e-record @16.7-16.32
			(fields
				(field (name "field")
					(e-string @16.16-16.20
						(e-literal @16.17-16.19 (string "hi"))))
				(field (name "other")
					(e-int @16.29-16.30 (value "5")))))
		(annotation @16.1-16.4
			(declared-type
				(ty @15.7-15.16 (name "BadRecord")))))
	(d-let
		(p-assign @21.1-21.4 (ident "qux"))
		(e-lambda @21.7-21.12
			(args
				(p-assign @21.8-21.9 (ident "x")))
			(e-lookup-local @21.11-21.12
				(p-assign @21.8-21.9 (ident "x"))))
		(annotation @21.1-21.4
			(declared-type
				(ty @20.7-20.18 (name "BadFunction")))))
	(d-let
		(p-assign @26.1-26.5 (ident "quux"))
		(e-tuple @26.8-26.21
			(elems
				(e-string @26.9-26.16
					(e-literal @26.10-26.15 (string "hello")))
				(e-int @26.18-26.20 (value "42"))))
		(annotation @26.1-26.5
			(declared-type
				(ty @25.8-25.16 (name "BadTuple")))))
	(s-nominal-decl @3.1-3.13
		(ty-header @3.1-3.8 (name "BadType"))
		(ty-underscore @1.1-1.1))
	(s-nominal-decl @8.1-8.19
		(ty-header @8.1-8.8 (name "BadList"))
		(ty-apply @8.12-8.19 (symbol "List")
			(ty-underscore @8.17-8.17)))
	(s-nominal-decl @13.1-13.38
		(ty-header @13.1-13.10 (name "BadRecord"))
		(ty-record @13.14-13.38
			(field (field "field")
				(ty-underscore @1.1-1.1))
			(field (field "other")
				(ty @13.33-13.36 (name "U32")))))
	(s-nominal-decl @18.1-18.22
		(ty-header @18.1-18.12 (name "BadFunction"))
		(ty-fn @18.16-18.22 (effectful false)
			(ty-underscore @1.1-1.1)
			(ty-underscore @1.1-1.1)))
	(s-nominal-decl @23.1-23.21
		(ty-header @23.1-23.9 (name "BadTuple"))
		(ty-tuple @23.13-23.21
			(ty-underscore @23.14-23.14)
			(ty @23.17-23.20 (name "U32")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.4 (type "Error"))
		(patt @11.1-11.4 (type "Error"))
		(patt @16.1-16.4 (type "Error"))
		(patt @21.1-21.4 (type "Error"))
		(patt @26.1-26.5 (type "Error")))
	(type_decls
		(nominal @3.1-3.13 (type "Error")
			(ty-header @3.1-3.8 (name "BadType")))
		(nominal @8.1-8.19 (type "Error")
			(ty-header @8.1-8.8 (name "BadList")))
		(nominal @13.1-13.38 (type "Error")
			(ty-header @13.1-13.10 (name "BadRecord")))
		(nominal @18.1-18.22 (type "Error")
			(ty-header @18.1-18.12 (name "BadFunction")))
		(nominal @23.1-23.21 (type "Error")
			(ty-header @23.1-23.9 (name "BadTuple"))))
	(expressions
		(expr @6.7-6.9 (type "Error"))
		(expr @11.7-11.16 (type "Error"))
		(expr @16.7-16.32 (type "Error"))
		(expr @21.7-21.12 (type "Error"))
		(expr @26.8-26.21 (type "Error"))))
~~~
