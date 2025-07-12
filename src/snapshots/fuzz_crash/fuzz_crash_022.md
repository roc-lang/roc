# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app [main!] { |f: platform "c" }

UserId : U64

ser : UserId -> Str
getUser = |id| if (id > 1!) "big" else "l"

-ain! = |_| getUser(900)
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_022.md:1:1:1:4
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_022.md:1:19:1:27
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_022.md:1:32:1:33
PARSE ERROR - fuzz_crash_022.md:6:27:6:28
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_022.md:8:1:8:2
INVALID STATEMENT - fuzz_crash_022.md:1:28:1:31
INVALID STATEMENT - fuzz_crash_022.md:1:32:1:33
UNUSED VARIABLE - fuzz_crash_022.md:6:12:6:14
INVALID STATEMENT - fuzz_crash_022.md:8:1:8:2
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_package_or_platform_name`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_022.md:1:1:1:4:**
```roc
app [main!] { |f: platform "c" }
```
^^^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **platform** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**fuzz_crash_022.md:1:19:1:27:**
```roc
app [main!] { |f: platform "c" }
```
                  ^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_022.md:1:32:1:33:**
```roc
app [main!] { |f: platform "c" }
```
                               ^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_022.md:6:27:6:28:**
```roc
getUser = |id| if (id > 1!) "big" else "l"
```
                          ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **-** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_022.md:8:1:8:2:**
```roc
-ain! = |_| getUser(900)
```
^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_022.md:1:28:1:31:**
```roc
app [main!] { |f: platform "c" }
```
                           ^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_022.md:1:32:1:33:**
```roc
app [main!] { |f: platform "c" }
```
                               ^


**INVALID IF CONDITION**
The condition in this `if` expression could not be processed.

The condition must be a valid expression that evaluates to a `Bool` value (`Bool.true` or `Bool.false`).

**UNUSED VARIABLE**
Variable ``id`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_id` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_022.md:6:12:6:14:**
```roc
getUser = |id| if (id > 1!) "big" else "l"
```
           ^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_022.md:8:1:8:2:**
```roc
-ain! = |_| getUser(900)
```
^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),OpBar(1:15-1:16),LowerIdent(1:16-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:30),StringEnd(1:30-1:31),CloseCurly(1:32-1:33),
UpperIdent(3:1-3:7),OpColon(3:8-3:9),UpperIdent(3:10-3:13),
LowerIdent(5:1-5:4),OpColon(5:5-5:6),UpperIdent(5:7-5:13),OpArrow(5:14-5:16),UpperIdent(5:17-5:20),
LowerIdent(6:1-6:8),OpAssign(6:9-6:10),OpBar(6:11-6:12),LowerIdent(6:12-6:14),OpBar(6:14-6:15),KwIf(6:16-6:18),OpenRound(6:19-6:20),LowerIdent(6:20-6:22),OpGreaterThan(6:23-6:24),Int(6:25-6:26),OpBang(6:26-6:27),CloseRound(6:27-6:28),StringStart(6:29-6:30),StringPart(6:30-6:33),StringEnd(6:33-6:34),KwElse(6:35-6:39),StringStart(6:40-6:41),StringPart(6:41-6:42),StringEnd(6:42-6:43),
OpUnaryMinus(8:1-8:2),LowerIdent(8:2-8:6),OpAssign(8:7-8:8),OpBar(8:9-8:10),Underscore(8:10-8:11),OpBar(8:11-8:12),LowerIdent(8:13-8:20),NoSpaceOpenRound(8:20-8:21),Int(8:21-8:24),CloseRound(8:24-8:25),EndOfFile(8:25-8:25),
~~~
# PARSE
~~~clojure
(file @1.1-8.25
	(malformed-header @1.1-1.16 (tag "expected_package_or_platform_name"))
	(statements
		(s-type-anno @1.16-1.27 (name "f")
			(ty-malformed @1.19-1.27 (tag "ty_anno_unexpected_token")))
		(e-string @1.28-1.31
			(e-string-part @1.29-1.30 (raw "c")))
		(e-malformed @1.32-1.33 (reason "expr_unexpected_token"))
		(s-type-decl @3.1-3.13
			(header @3.1-3.7 (name "UserId")
				(args))
			(ty @3.10-3.13 (name "U64")))
		(s-type-anno @5.1-5.20 (name "ser")
			(ty-fn @5.7-5.20
				(ty @5.7-5.13 (name "UserId"))
				(ty @5.17-5.20 (name "Str"))))
		(s-decl @6.1-6.43
			(p-ident @6.1-6.8 (raw "getUser"))
			(e-lambda @6.11-6.43
				(args
					(p-ident @6.12-6.14 (raw "id")))
				(e-if-then-else @6.16-6.43
					(e-malformed @6.27-6.28 (reason "expected_expr_close_round_or_comma"))
					(e-string @6.29-6.34
						(e-string-part @6.30-6.33 (raw "big")))
					(e-string @6.40-6.43
						(e-string-part @6.41-6.42 (raw "l"))))))
		(e-malformed @8.1-8.2 (reason "expr_unexpected_token"))
		(s-decl @8.2-8.25
			(p-ident @8.2-8.6 (raw "ain!"))
			(e-lambda @8.9-8.25
				(args
					(p-underscore))
				(e-apply @8.13-8.25
					(e-ident @8.13-8.20 (raw "getUser"))
					(e-int @8.21-8.24 (raw "900")))))))
~~~
# FORMATTED
~~~roc
f : "c"

UserId : U64

ser : UserId -> Str
getUser = |id| if  "big" else "l"

ain! = |_| getUser(900)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.8 (ident "getUser"))
		(e-lambda @6.11-6.43
			(args
				(p-assign @6.12-6.14 (ident "id")))
			(e-if @6.16-6.43
				(if-branches
					(if-branch
						(e-runtime-error (tag "if_condition_not_canonicalized"))
						(e-string @6.29-6.34
							(e-literal @6.30-6.33 (string "big")))))
				(if-else
					(e-string @6.40-6.43
						(e-literal @6.41-6.42 (string "l")))))))
	(d-let
		(p-assign @8.2-8.6 (ident "ain!"))
		(e-lambda @8.9-8.25
			(args
				(p-underscore @8.10-8.11))
			(e-call @8.13-8.25
				(e-lookup-local @8.13-8.20
					(p-assign @6.1-6.8 (ident "getUser")))
				(e-int @8.21-8.24 (value "900")))))
	(s-alias-decl @3.1-3.13
		(ty-header @3.1-3.7 (name "UserId"))
		(ty @3.10-3.13 (name "U64"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.8 (type "* -> Str"))
		(patt @8.2-8.6 (type "* -> Str")))
	(type_decls
		(alias @3.1-3.13 (type "UserId")
			(ty-header @3.1-3.7 (name "UserId"))))
	(expressions
		(expr @6.11-6.43 (type "* -> Str"))
		(expr @8.9-8.25 (type "* -> Str"))))
~~~
