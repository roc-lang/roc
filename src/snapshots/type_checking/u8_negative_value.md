# META
~~~ini
description=U8 type annotation with negative value
type=file
~~~
# SOURCE
~~~roc
module []

x : U8
x = -1
~~~
# PROBLEMS
**NEGATIVE UNSIGNED INTEGER**
The number **-1** is **signed** because it is negative:
**u8_negative_value.md:4:5:4:7:**
```roc
x = -1
```

However, its inferred type is **unsigned**:
    _U8_

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:2),OpColon(3:3-3:4),UpperIdent(3:5-3:7),Newline(1:1-1:1),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),Int(4:5-4:7),EndOfFile(4:7-4:7),
~~~
# PARSE
~~~clojure
(file @1-1-4-7
	(module @1-1-1-10
		(exposes @1-8-1-10))
	(statements
		(s-type-anno @3-1-4-2 (name "x")
			(ty (name "U8")))
		(s-decl @4-1-4-7
			(p-ident @4-1-4-2 (raw "x"))
			(e-int @4-5-4-7 (raw "-1")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "x") (type "Error")))
	(expressions
		(expr @4-5-4-7 (type "Error"))))
~~~