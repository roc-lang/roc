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
UNDECLARED TYPE - type_undeclared_usage.md:8:25:8:37
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


**UNDECLARED TYPE**
The type _SomeModule.MissingType_ is not declared in this scope.

This type is referenced here:
**type_undeclared_usage.md:8:25:8:37:**
```roc
AnotherType : SomeModule.MissingType
```
                        ^^^^^^^^^^^^


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
UpperIdent,OpColon,UpperIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
StringStart,StringPart,StringEnd,
CloseCurly,
UpperIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "MyType")
				(args))
			(ty (name "UnknownType")))
		(s-type-anno (name "processValue")
			(ty-fn
				(ty (name "UndeclaredResult"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "processValue"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-block
					(statements
						(e-string
							(e-string-part (raw "processed")))))))
		(s-type-decl
			(header (name "AnotherType")
				(args))
			(ty (name "SomeModule.MissingType")))))
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
		(p-assign (ident "processValue"))
		(e-lambda
			(args
				(p-assign (ident "value")))
			(e-block
				(e-string
					(e-literal (string "processed")))))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (builtin)))))
	(s-alias-decl
		(ty-header (name "MyType"))
		(ty-malformed))
	(s-alias-decl
		(ty-header (name "AnotherType"))
		(ty-malformed)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> Str")))
	(type_decls
		(alias (type "MyType")
			(ty-header (name "MyType")))
		(alias (type "AnotherType")
			(ty-header (name "AnotherType"))))
	(expressions
		(expr (type "Error -> Str"))))
~~~
