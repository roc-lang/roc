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
# PROBLEMS
**UNDECLARED TYPE**
The type ``UnknownType`` is not declared in this scope.

This type is referenced here:
**type_undeclared_usage.md:3:10:3:21:**
```roc
MyType : UnknownType
```


**UNDECLARED TYPE**
The type ``UndeclaredResult`` is not declared in this scope.

This type is referenced here:
**type_undeclared_usage.md:5:16:5:32:**
```roc
processValue : UndeclaredResult -> Str
```


**UNUSED VARIABLE**
Variable ``value`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_value` to suppress this warning.
The unused variable is declared here:
**type_undeclared_usage.md:6:17:6:22:**
```roc
processValue = |value| {
```


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:15),Comma(1:15-1:16),LowerIdent(1:17-1:29),CloseSquare(1:29-1:30),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:7),OpColon(3:8-3:9),UpperIdent(3:10-3:21),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:13),OpColon(5:14-5:15),UpperIdent(5:16-5:32),OpArrow(5:33-5:35),UpperIdent(5:36-5:39),Newline(1:1-1:1),
LowerIdent(6:1-6:13),OpAssign(6:14-6:15),OpBar(6:16-6:17),LowerIdent(6:17-6:22),OpBar(6:22-6:23),OpenCurly(6:24-6:25),Newline(1:1-1:1),
StringStart(7:5-7:6),StringPart(7:6-7:15),StringEnd(7:15-7:16),Newline(1:1-1:1),
CloseCurly(8:1-8:2),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(10:1-10:12),OpColon(10:13-10:14),UpperIdent(10:15-10:25),NoSpaceDotUpperIdent(10:25-10:37),EndOfFile(10:37-10:37),
~~~
# PARSE
~~~clojure
(file (1:1-10:37)
	(module (1:1-1:30)
		(exposes (1:8-1:30)
			(exposed_item (upper_ident "MyType"))
			(exposed_item (lower_ident "processValue"))))
	(statements
		(type_decl (3:1-5:13)
			(header (3:1-3:7) "MyType" (args))
			(ty "UnknownType"))
		(type_anno (5:1-6:13)
			"processValue"
			(fn (5:16-5:39)
				(ty "UndeclaredResult")
				(ty "Str")))
		(decl (6:1-8:2)
			(ident (6:1-6:13) "processValue")
			(lambda (6:16-8:2)
				(args (ident (6:17-6:22) "value"))
				(block (6:24-8:2)
					(statements
						(string (7:5-7:16) (string_part (7:6-7:15) "processed"))))))
		(type_decl (10:1-10:37)
			(header (10:1-10:12) "AnotherType" (args))
			(mod_ty "SomeModule" ".MissingType"))))
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
(can_ir
	(d_let
		(def_pattern
			(p_assign (6:1-6:13)
				(pid 83)
				(ident "processValue")))
		(def_expr
			(e_lambda (6:16-8:2)
				(args
					(p_assign (6:17-6:22)
						(pid 84)
						(ident "value")))
				(e_block (6:24-8:2)
					(e_string (7:5-7:16) (e_literal (7:6-7:15) "processed")))))
		(annotation (6:1-6:13)
			(signature 93)
			(declared_type
				(fn (5:16-5:39)
					(ty (5:16-5:32) "UndeclaredResult")
					(ty (5:36-5:39) "Str")
					"false"))))
	(s_type_decl (3:1-5:13)
		(type_header (3:1-3:7) "MyType")
		(ty (3:10-3:21) "UnknownType"))
	(s_type_decl (10:1-10:37)
		(type_header (10:1-10:12) "AnotherType")
		(mod_ty (10:15-10:37) "MissingType" "SomeModule")))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "processValue" 95 (type "*")))
	(expressions
		(expr (6:16-8:2) 88 (type "*"))))
~~~