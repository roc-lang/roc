# META
~~~ini
description=Two decls
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

a = 5
b = a + 1
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - can_two_decls.md:5:1:5:3
UNEXPECTED TOKEN IN EXPRESSION - can_two_decls.md:5:2:5:4
UNEXPECTED TOKEN IN EXPRESSION - can_two_decls.md:5:3:5:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**can_two_decls.md:5:1:5:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**can_two_decls.md:5:2:5:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**can_two_decls.md:5:3:5:4:**
```roc
~~~
```
  ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),Int(3:5-3:6),Newline(1:1-1:1),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),LowerIdent(4:5-4:6),OpPlus(4:7-4:8),Int(4:9-4:10),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
~~~
# PARSE
~~~clojure
(file @1.1-5.4
	(app @1.1-1.57
		(provides @1.6-1.12
			(exposed-lower-ident (text "main!")))
		(record-field @1.15-1.57 (name "pf")
			(e-string @1.28-1.55
				(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))
		(packages @1.13-1.57
			(record-field @1.15-1.57 (name "pf")
				(e-string @1.28-1.55
					(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-decl @3.1-3.6
			(p-ident @3.1-3.2 (raw "a"))
			(e-int @3.5-3.6 (raw "5")))
		(s-decl @4.1-5.2
			(p-ident @4.1-4.2 (raw "b"))
			(e-binop @4.5-5.2 (op "+")
				(e-ident @4.5-4.6 (raw "a"))
				(e-int @4.9-4.10 (raw "1"))))
		(e-malformed @5.1-5.3 (reason "expr_unexpected_token"))
		(e-malformed @5.2-5.4 (reason "expr_unexpected_token"))
		(e-malformed @5.3-5.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

a = 5
b = a + 1

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.2 (ident "a"))
		(e-int @3.5-3.6 (value "5")))
	(d-let
		(p-assign @4.1-4.2 (ident "b"))
		(e-binop @4.5-5.2 (op "add")
			(e-lookup-local @4.5-4.6
				(pattern @3.1-3.2))
			(e-int @4.9-4.10 (value "1")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.2 (type "Num(*)"))
		(patt @4.1-4.2 (type "*")))
	(expressions
		(expr @3.5-3.6 (type "Num(*)"))
		(expr @4.5-5.2 (type "*"))))
~~~
