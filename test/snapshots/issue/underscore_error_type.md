# META
~~~ini
description=Type declarations with underscores should become error types that fail unification
type=snippet
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
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:6:12:6:16
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:21:14:21:14
TYPE MISMATCH - underscore_error_type.md:4:7:4:9
TYPE MISMATCH - underscore_error_type.md:9:7:9:16
TYPE MISMATCH - underscore_error_type.md:14:7:14:32
TYPE MISMATCH - underscore_error_type.md:19:7:19:12
TYPE MISMATCH - underscore_error_type.md:24:8:24:21
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

**underscore_error_type.md:6:12:6:16:**
```roc
BadList := List(_)
```
           ^^^^

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

**TYPE MISMATCH**
This expression is used in an unexpected way:
**underscore_error_type.md:4:7:4:9:**
```roc
foo = 42
```
      ^^

It has the type:
    _Num(_size)_

But the type annotation says it should have the type:
    _BadType_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**underscore_error_type.md:9:7:9:16:**
```roc
bar = [1, 2, 3]
```
      ^^^^^^^^^

It has the type:
    _List(Num(_size))_

But the type annotation says it should have the type:
    _BadList_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**underscore_error_type.md:14:7:14:32:**
```roc
baz = { field: "hi", other: 5 }
```
      ^^^^^^^^^^^^^^^^^^^^^^^^^

It has the type:
    _{ field: Str, other: Num(_size) }_

But the type annotation says it should have the type:
    _BadRecord_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**underscore_error_type.md:19:7:19:12:**
```roc
qux = |x| x
```
      ^^^^^

It has the type:
    _a -> a_

But the type annotation says it should have the type:
    _BadFunction_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**underscore_error_type.md:24:8:24:21:**
```roc
quux = ("hello", 42)
```
       ^^^^^^^^^^^^^

It has the type:
    _(Str, Num(_size))_

But the type annotation says it should have the type:
    _BadTuple_

# TOKENS
~~~zig
UpperIdent,OpColonEqual,Underscore,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,Underscore,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,CloseCurly,
UpperIdent,OpColonEqual,Underscore,OpArrow,Underscore,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
UpperIdent,OpColonEqual,OpenRound,Underscore,Comma,UpperIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenRound,StringStart,StringPart,StringEnd,Comma,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "BadType")
				(args))
			(_))
		(s-type-anno (name "foo")
			(ty (name "BadType")))
		(s-decl
			(p-ident (raw "foo"))
			(e-int (raw "42")))
		(s-type-decl
			(header (name "BadList")
				(args))
			(ty-apply
				(ty (name "List"))
				(_)))
		(s-type-anno (name "bar")
			(ty (name "BadList")))
		(s-decl
			(p-ident (raw "bar"))
			(e-list
				(e-int (raw "1"))
				(e-int (raw "2"))
				(e-int (raw "3"))))
		(s-type-decl
			(header (name "BadRecord")
				(args))
			(ty-record
				(anno-record-field (name "field")
					(_))
				(anno-record-field (name "other")
					(ty (name "U32")))))
		(s-type-anno (name "baz")
			(ty (name "BadRecord")))
		(s-decl
			(p-ident (raw "baz"))
			(e-record
				(field (field "field")
					(e-string
						(e-string-part (raw "hi"))))
				(field (field "other")
					(e-int (raw "5")))))
		(s-type-decl
			(header (name "BadFunction")
				(args))
			(ty-fn
				(_)
				(_)))
		(s-type-anno (name "qux")
			(ty (name "BadFunction")))
		(s-decl
			(p-ident (raw "qux"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x"))))
		(s-type-decl
			(header (name "BadTuple")
				(args))
			(ty-tuple
				(_)
				(ty (name "U32"))))
		(s-type-anno (name "quux")
			(ty (name "BadTuple")))
		(s-decl
			(p-ident (raw "quux"))
			(e-tuple
				(e-string
					(e-string-part (raw "hello")))
				(e-int (raw "42"))))))
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
		(p-assign (ident "foo"))
		(e-num (value "42"))
		(annotation
			(ty-lookup (name "BadType") (local))))
	(d-let
		(p-assign (ident "bar"))
		(e-list
			(elems
				(e-num (value "1"))
				(e-num (value "2"))
				(e-num (value "3"))))
		(annotation
			(ty-lookup (name "BadList") (local))))
	(d-let
		(p-assign (ident "baz"))
		(e-record
			(fields
				(field (name "field")
					(e-string
						(e-literal (string "hi"))))
				(field (name "other")
					(e-num (value "5")))))
		(annotation
			(ty-lookup (name "BadRecord") (local))))
	(d-let
		(p-assign (ident "qux"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x"))))
		(annotation
			(ty-lookup (name "BadFunction") (local))))
	(d-let
		(p-assign (ident "quux"))
		(e-tuple
			(elems
				(e-string
					(e-literal (string "hello")))
				(e-num (value "42"))))
		(annotation
			(ty-lookup (name "BadTuple") (local))))
	(s-nominal-decl
		(ty-header (name "BadType"))
		(ty-underscore))
	(s-nominal-decl
		(ty-header (name "BadList"))
		(ty-apply (name "List") (builtin)
			(ty-underscore)))
	(s-nominal-decl
		(ty-header (name "BadRecord"))
		(ty-record
			(field (field "field")
				(ty-underscore))
			(field (field "other")
				(ty-lookup (name "U32") (builtin)))))
	(s-nominal-decl
		(ty-header (name "BadFunction"))
		(ty-fn (effectful false)
			(ty-underscore)
			(ty-underscore)))
	(s-nominal-decl
		(ty-header (name "BadTuple"))
		(ty-tuple
			(ty-underscore)
			(ty-lookup (name "U32") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error")))
	(type_decls
		(nominal (type "BadType")
			(ty-header (name "BadType")))
		(nominal (type "BadList")
			(ty-header (name "BadList")))
		(nominal (type "BadRecord")
			(ty-header (name "BadRecord")))
		(nominal (type "BadFunction")
			(ty-header (name "BadFunction")))
		(nominal (type "BadTuple")
			(ty-header (name "BadTuple"))))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
