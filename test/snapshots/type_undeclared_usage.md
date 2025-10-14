# META
~~~ini
description=Undeclared type usage should produce error
type=snippet
~~~
# SOURCE
~~~roc
MyType : UnknownType

processValue : UndeclaredResult -> Str
processValue = |value| {
    "processed"
}

AnotherType : SomeModule.MissingType
~~~
# EXPECTED
UNDECLARED TYPE - type_undeclared_usage.md:1:10:1:21
MODULE NOT IMPORTED - type_undeclared_usage.md:8:15:8:37
UNDECLARED TYPE - type_undeclared_usage.md:3:16:3:32
UNUSED VARIABLE - type_undeclared_usage.md:4:17:4:22
# PROBLEMS
**UNDECLARED TYPE**
The type _UnknownType_ is not declared in this scope.

This type is referenced here:
**type_undeclared_usage.md:1:10:1:21:**
```roc
MyType : UnknownType
```
         ^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `SomeModule` imported into this Roc file.

You're attempting to use this module here:
**type_undeclared_usage.md:8:15:8:37:**
```roc
AnotherType : SomeModule.MissingType
```
              ^^^^^^^^^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _UndeclaredResult_ is not declared in this scope.

This type is referenced here:
**type_undeclared_usage.md:3:16:3:32:**
```roc
processValue : UndeclaredResult -> Str
```
               ^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `value` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_value` to suppress this warning.
The unused variable is declared here:
**type_undeclared_usage.md:4:17:4:22:**
```roc
processValue = |value| {
```
                ^^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:7),OpColon(1:8-1:9),UpperIdent(1:10-1:21),
LowerIdent(3:1-3:13),OpColon(3:14-3:15),UpperIdent(3:16-3:32),OpArrow(3:33-3:35),UpperIdent(3:36-3:39),
LowerIdent(4:1-4:13),OpAssign(4:14-4:15),OpBar(4:16-4:17),LowerIdent(4:17-4:22),OpBar(4:22-4:23),OpenCurly(4:24-4:25),
StringStart(5:5-5:6),StringPart(5:6-5:15),StringEnd(5:15-5:16),
CloseCurly(6:1-6:2),
UpperIdent(8:1-8:12),OpColon(8:13-8:14),UpperIdent(8:15-8:25),NoSpaceDotUpperIdent(8:25-8:37),
EndOfFile(9:1-9:1),
~~~
# PARSE
~~~clojure
(file @1.1-8.37
	(type-module @1.1-1.7)
	(statements
		(s-type-decl @1.1-1.21
			(header @1.1-1.7 (name "MyType")
				(args))
			(ty @1.10-1.21 (name "UnknownType")))
		(s-type-anno @3.1-3.39 (name "processValue")
			(ty-fn @3.16-3.39
				(ty @3.16-3.32 (name "UndeclaredResult"))
				(ty @3.36-3.39 (name "Str"))))
		(s-decl @4.1-6.2
			(p-ident @4.1-4.13 (raw "processValue"))
			(e-lambda @4.16-6.2
				(args
					(p-ident @4.17-4.22 (raw "value")))
				(e-block @4.24-6.2
					(statements
						(e-string @5.5-5.16
							(e-string-part @5.6-5.15 (raw "processed")))))))
		(s-type-decl @8.1-8.37
			(header @8.1-8.12 (name "AnotherType")
				(args))
			(ty @8.15-8.37 (name "SomeModule.MissingType")))))
~~~
# FORMATTED
~~~roc
MyType : UnknownType

processValue : UndeclaredResult -> Str
processValue = |value| {
	"processed"
}

AnotherType : SomeModule.MissingType
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.13 (ident "processValue"))
		(e-lambda @4.16-6.2
			(args
				(p-assign @4.17-4.22 (ident "value")))
			(e-block @4.24-6.2
				(e-string @5.5-5.16
					(e-literal @5.6-5.15 (string "processed")))))
		(annotation @4.1-4.13
			(declared-type
				(ty-fn @3.16-3.39 (effectful false)
					(ty-malformed @3.16-3.32)
					(ty-lookup @3.36-3.39 (name "Str") (builtin))))))
	(s-alias-decl @1.1-1.21
		(ty-header @1.1-1.7 (name "MyType"))
		(ty-malformed @1.10-1.21))
	(s-alias-decl @8.1-8.37
		(ty-header @8.1-8.12 (name "AnotherType"))
		(ty-malformed @8.15-8.37)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.13 (type "Error -> Str")))
	(type_decls
		(alias @1.1-1.21 (type "MyType")
			(ty-header @1.1-1.7 (name "MyType")))
		(alias @8.1-8.37 (type "AnotherType")
			(ty-header @8.1-8.12 (name "AnotherType"))))
	(expressions
		(expr @4.16-6.2 (type "Error -> Str"))))
~~~
