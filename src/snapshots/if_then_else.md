# META
~~~ini
description=Example if-then-else statement
type=file
~~~
# SOURCE
~~~roc
module [foo]

foo = if 1 A

    else {
	"hello"
    }
~~~
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**if_then_else.md:3:10:3:11:**
```roc
foo = if 1 A
```
         ^

It is of type:
    _Num(*)_

But you are trying to use it as:
    _[True, False]_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**if_then_else.md:5:10:7:6:**
```roc
    else {
	"hello"
    }
```

It is of type:
    _Str_

But you are trying to use it as:
    _[A]*_

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),KwIf(3:7-3:9),Int(3:10-3:11),UpperIdent(3:12-3:13),Newline(1:1-1:1),
Newline(1:1-1:1),
KwElse(5:5-5:9),OpenCurly(5:10-5:11),Newline(1:1-1:1),
StringStart(6:2-6:3),StringPart(6:3-6:8),StringEnd(6:8-6:9),Newline(1:1-1:1),
CloseCurly(7:5-7:6),EndOfFile(7:6-7:6),
~~~
# PARSE
~~~clojure
(file @1.1-7.6
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident (text "foo"))))
	(statements
		(s-decl @3.1-7.6
			(p-ident @3.1-3.4 (raw "foo"))
			(e-if-then-else @3.7-7.6
				(e-int @3.10-3.11 (raw "1"))
				(e-tag @3.12-3.13 (raw "A"))
				(e-block @5.10-7.6
					(statements
						(e-string @6.2-6.9
							(e-string-part @6.3-6.8 (raw "hello")))))))))
~~~
# FORMATTED
~~~roc
module [foo]

foo = if 1 A

	else {
		"hello"
	}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 82)
		(p-assign @3.1-3.4 (ident "foo") (id 73))
		(e-if @3.7-7.6 (id 81)
			(if-branches
				(if-branch
					(e-int @3.10-3.11 (value "1"))
					(e-tag @3.12-3.13 (ext-var 75) (name "A") (args "TODO"))))
			(if-else
				(e-block @5.10-7.6
					(e-string @6.2-6.9
						(e-literal @6.3-6.8 (string "hello"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(d_assign (name "foo") (def_var 82) (type "Error")))
	(expressions
		(expr @3.7-7.6 (type "Error"))))
~~~
