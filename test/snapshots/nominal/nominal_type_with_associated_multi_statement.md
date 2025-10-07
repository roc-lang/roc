# META
~~~ini
description=Nominal type with multi-statement associated items
type=snippet
~~~
# SOURCE
~~~roc
Foo := [A, B, C].{
    x = 5
    y = 10
    z = 15
}
~~~
# EXPECTED
UNUSED VARIABLE - nominal_type_with_associated_multi_statement.md:2:5:2:10
UNUSED VARIABLE - nominal_type_with_associated_multi_statement.md:3:5:3:11
UNUSED VARIABLE - nominal_type_with_associated_multi_statement.md:4:5:4:11
# PROBLEMS
**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**nominal_type_with_associated_multi_statement.md:2:5:2:10:**
```roc
    x = 5
```
    ^^^^^


**UNUSED VARIABLE**
Variable `y` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_y` to suppress this warning.
The unused variable is declared here:
**nominal_type_with_associated_multi_statement.md:3:5:3:11:**
```roc
    y = 10
```
    ^^^^^^


**UNUSED VARIABLE**
Variable `z` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_z` to suppress this warning.
The unused variable is declared here:
**nominal_type_with_associated_multi_statement.md:4:5:4:11:**
```roc
    z = 15
```
    ^^^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:10),Comma(1:10-1:11),UpperIdent(1:12-1:13),Comma(1:13-1:14),UpperIdent(1:15-1:16),CloseSquare(1:16-1:17),Dot(1:17-1:18),OpenCurly(1:18-1:19),
LowerIdent(2:5-2:6),OpAssign(2:7-2:8),Int(2:9-2:10),
LowerIdent(3:5-3:6),OpAssign(3:7-3:8),Int(3:9-3:11),
LowerIdent(4:5-4:6),OpAssign(4:7-4:8),Int(4:9-4:11),
CloseCurly(5:1-5:2),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.2
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-5.2
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.17
				(tags
					(ty @1.9-1.10 (name "A"))
					(ty @1.12-1.13 (name "B"))
					(ty @1.15-1.16 (name "C"))))
			(associated @1.18-5.2
				(s-decl @2.5-2.10
					(p-ident @2.5-2.6 (raw "x"))
					(e-int @2.9-2.10 (raw "5")))
				(s-decl @3.5-3.11
					(p-ident @3.5-3.6 (raw "y"))
					(e-int @3.9-3.11 (raw "10")))
				(s-decl @4.5-4.11
					(p-ident @4.5-4.6 (raw "z"))
					(e-int @4.9-4.11 (raw "15")))))))
~~~
# FORMATTED
~~~roc
Foo := [A, B, C].{
	x = 5
	y = 10
	z = 15
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.5-2.10 (ident "Foo.x"))
		(e-num @2.9-2.10 (value "5")))
	(d-let
		(p-assign @3.5-3.11 (ident "Foo.y"))
		(e-num @3.9-3.11 (value "10")))
	(d-let
		(p-assign @4.5-4.11 (ident "Foo.z"))
		(e-num @4.9-4.11 (value "15")))
	(s-nominal-decl @1.1-5.2
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.17
			(ty-tag-name @1.9-1.10 (name "A"))
			(ty-tag-name @1.12-1.13 (name "B"))
			(ty-tag-name @1.15-1.16 (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.5-2.10 (type "Num(_size)"))
		(patt @3.5-3.11 (type "Num(_size)"))
		(patt @4.5-4.11 (type "Num(_size)")))
	(type_decls
		(nominal @1.1-5.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo"))))
	(expressions
		(expr @2.9-2.10 (type "Num(_size)"))
		(expr @3.9-3.11 (type "Num(_size)"))
		(expr @4.9-4.11 (type "Num(_size)"))))
~~~
