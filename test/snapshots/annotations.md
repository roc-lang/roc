# META
~~~ini
description=Example of a nominal tag union with a payload
type=file
~~~
# SOURCE
~~~roc
module []

x =
  match "hello" {
    True => "true"
    False => "false"
  }
~~~
# EXPECTED
TYPE MISMATCH - annotations.md:18:28:18:28
INVALID NOMINAL TAG - annotations.md:21:22:21:41
INVALID NOMINAL TAG - annotations.md:24:24:24:39
TYPE MISMATCH - annotations.md:28:35:28:35
# PROBLEMS
**INCOMPATIBLE MATCH PATTERNS**
The first pattern in this `match` is incompatible:
**annotations.md:4:3:**
```roc
  match "hello" {
    True => "true"
    False => "false"
  }
```
    ^^^^

The first pattern has the type:
    _Bool_

But the expression right after `match` has the type:
    _Str_

These two types can't never match!



# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),
KwMatch(4:3-4:8),StringStart(4:9-4:10),StringPart(4:10-4:15),StringEnd(4:15-4:16),OpenCurly(4:17-4:18),
UpperIdent(5:5-5:9),OpFatArrow(5:10-5:12),StringStart(5:13-5:14),StringPart(5:14-5:18),StringEnd(5:18-5:19),
UpperIdent(6:5-6:10),OpFatArrow(6:11-6:13),StringStart(6:14-6:15),StringPart(6:15-6:20),StringEnd(6:20-6:21),
CloseCurly(7:3-7:4),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.4
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-decl @3.1-7.4
			(p-ident @3.1-3.2 (raw "x"))
			(e-match
				(e-string @4.9-4.16
					(e-string-part @4.10-4.15 (raw "hello")))
				(branches
					(branch @5.5-5.19
						(p-tag @5.5-5.9 (raw "True"))
						(e-string @5.13-5.19
							(e-string-part @5.14-5.18 (raw "true"))))
					(branch @6.5-6.21
						(p-tag @6.5-6.10 (raw "False"))
						(e-string @6.14-6.21
							(e-string-part @6.15-6.20 (raw "false")))))))))
~~~
# FORMATTED
~~~roc
module []

x = 
	match "hello" {
		True => "true"
		False => "false"
	}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.2 (ident "x"))
		(e-match @4.3-7.4
			(match @4.3-7.4
				(cond
					(e-string @4.9-4.16
						(e-literal @4.10-4.15 (string "hello"))))
				(branches
					(branch
						(patterns
							(pattern (degenerate false)
								(p-nominal @5.5-5.9
									(p-applied-tag @5.5-5.9))))
						(value
							(e-string @5.13-5.19
								(e-literal @5.14-5.18 (string "true")))))
					(branch
						(patterns
							(pattern (degenerate false)
								(p-nominal @6.5-6.10
									(p-applied-tag @6.5-6.10))))
						(value
							(e-string @6.14-6.21
								(e-literal @6.15-6.20 (string "false")))))))))
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
			(tag_name @1.1-1.1 (name "Err")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.2 (type "Str")))
	(type_decls
		(nominal @1.1-1.1 (type "Error")
			(ty-header @1.1-1.1 (name "Bool")))
		(nominal @1.1-1.1 (type "Result(ok, err)")
			(ty-header @1.1-1.1 (name "Result")
				(ty-args
					(ty-rigid-var @1.1-1.1 (name "ok"))
					(ty-rigid-var @1.1-1.1 (name "err"))))))
	(expressions
		(expr @4.3-7.4 (type "Str"))))
~~~
