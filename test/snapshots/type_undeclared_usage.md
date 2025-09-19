# META
~~~ini
description=Undeclared type usage should produce error
type=file
~~~
# SOURCE
~~~roc
module [MyType, processValue]

MyType : UnknownType

processValue : UndeclaredResult -> Str
processValue = |value| {
    "processed"
}

AnotherType : SomeModule.MissingType
~~~
# EXPECTED
UNDECLARED TYPE - type_undeclared_usage.md:3:10:3:21
MODULE NOT IMPORTED - type_undeclared_usage.md:10:15:10:37
UNDECLARED TYPE - type_undeclared_usage.md:5:16:5:32
UNUSED VARIABLE - type_undeclared_usage.md:6:17:6:22
# PROBLEMS
**UNDECLARED TYPE**
The type _UnknownType_ is not declared in this scope.

This type is referenced here:
**type_undeclared_usage.md:3:10:3:21:**
```roc
MyType : UnknownType
```
         ^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `SomeModule` imported into this Roc file.

You're attempting to use this module here:
**type_undeclared_usage.md:10:15:10:37:**
```roc
AnotherType : SomeModule.MissingType
```
              ^^^^^^^^^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _UndeclaredResult_ is not declared in this scope.

This type is referenced here:
**type_undeclared_usage.md:5:16:5:32:**
```roc
processValue : UndeclaredResult -> Str
```
               ^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `value` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_value` to suppress this warning.
The unused variable is declared here:
**type_undeclared_usage.md:6:17:6:22:**
```roc
processValue = |value| {
```
                ^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:15),Comma(1:15-1:16),LowerIdent(1:17-1:29),CloseSquare(1:29-1:30),
UpperIdent(3:1-3:7),OpColon(3:8-3:9),UpperIdent(3:10-3:21),
LowerIdent(5:1-5:13),OpColon(5:14-5:15),UpperIdent(5:16-5:32),OpArrow(5:33-5:35),UpperIdent(5:36-5:39),
LowerIdent(6:1-6:13),OpAssign(6:14-6:15),OpBar(6:16-6:17),LowerIdent(6:17-6:22),OpBar(6:22-6:23),OpenCurly(6:24-6:25),
StringStart(7:5-7:6),StringPart(7:6-7:15),StringEnd(7:15-7:16),
CloseCurly(8:1-8:2),
UpperIdent(10:1-10:12),OpColon(10:13-10:14),UpperIdent(10:15-10:25),NoSpaceDotUpperIdent(10:25-10:37),
EndOfFile(11:1-11:1),
~~~
# PARSE
~~~clojure
(file @1.1-10.37
	(module @1.1-1.30
		(exposes @1.8-1.30
			(exposed-upper-ident @1.9-1.15 (text "MyType"))
			(exposed-lower-ident @1.17-1.29
				(text "processValue"))))
	(statements
		(s-type-decl @3.1-3.21
			(header @3.1-3.7 (name "MyType")
				(args))
			(ty @3.10-3.21 (name "UnknownType")))
		(s-type-anno @5.1-5.39 (name "processValue")
			(ty-fn @5.16-5.39
				(ty @5.16-5.32 (name "UndeclaredResult"))
				(ty @5.36-5.39 (name "Str"))))
		(s-decl @6.1-8.2
			(p-ident @6.1-6.13 (raw "processValue"))
			(e-lambda @6.16-8.2
				(args
					(p-ident @6.17-6.22 (raw "value")))
				(e-block @6.24-8.2
					(statements
						(e-string @7.5-7.16
							(e-string-part @7.6-7.15 (raw "processed")))))))
		(s-type-decl @10.1-10.37
			(header @10.1-10.12 (name "AnotherType")
				(args))
			(ty @10.15-10.37 (name "SomeModule.MissingType")))))
~~~
# FORMATTED
~~~roc
module [MyType, processValue]

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
		(p-assign @6.1-6.13 (ident "processValue"))
		(e-lambda @6.16-8.2
			(args
				(p-assign @6.17-6.22 (ident "value")))
			(e-block @6.24-8.2
				(e-string @7.5-7.16
					(e-literal @7.6-7.15 (string "processed")))))
		(annotation @6.1-6.13
			(declared-type
				(ty-fn @5.16-5.39 (effectful false)
					(ty-malformed @5.16-5.32)
					(ty-lookup @5.36-5.39 (name "Str") (builtin))))))
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Bool"))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "True"))
			(tag_name @1.1-1.1 (name "False"))))
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Result")
			(ty-args
				(ty-rigid-var @1.1-1.1 (name "ok"))
				(ty-rigid-var @1.1-1.1 (name "err"))))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "Ok"))
			(tag_name @1.1-1.1 (name "Err"))))
	(s-alias-decl @3.1-3.21
		(ty-header @3.1-3.7 (name "MyType"))
		(ty-malformed @3.10-3.21))
	(s-alias-decl @10.1-10.37
		(ty-header @10.1-10.12 (name "AnotherType"))
		(ty-malformed @10.15-10.37)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.13 (type "Error -> Str")))
	(type_decls
		(nominal @1.1-1.1 (type "Bool")
			(ty-header @1.1-1.1 (name "Bool")))
		(nominal @1.1-1.1 (type "Result(ok, err)")
			(ty-header @1.1-1.1 (name "Result")
				(ty-args
					(ty-rigid-var @1.1-1.1 (name "ok"))
					(ty-rigid-var @1.1-1.1 (name "err")))))
		(alias @3.1-3.21 (type "MyType")
			(ty-header @3.1-3.7 (name "MyType")))
		(alias @10.1-10.37 (type "AnotherType")
			(ty-header @10.1-10.12 (name "AnotherType"))))
	(expressions
		(expr @6.16-8.2 (type "Error -> Str"))))
~~~
