# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app
[main!] { p\\   @\\-\\\Fc" }

UserId : U64

ser : UserId

9el
va: {
    Res -> Str
getUs=

 ,  ]er = |id| if (id > 10) "big" else   @

ma  ! = |_|    ser(100)
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
ASCII CONTROL CHARACTER - :0:0:0:0
ASCII CONTROL CHARACTER - :0:0:0:0
UNCLOSED STRING - :0:0:0:0
ASCII CONTROL CHARACTER - :0:0:0:0
ASCII CONTROL CHARACTER - :0:0:0:0
ASCII CONTROL CHARACTER - :0:0:0:0
ASCII CONTROL CHARACTER - :0:0:0:0
ASCII CONTROL CHARACTER - :0:0:0:0
ASCII CONTROL CHARACTER - :0:0:0:0
ASCII CONTROL CHARACTER - :0:0:0:0
ASCII CONTROL CHARACTER - :0:0:0:0
PARSE ERROR - fuzz_crash_026.md:1:1:1:4
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_026.md:2:13:2:14
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_026.md:2:17:2:18
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_026.md:2:18:2:19
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_026.md:2:19:2:20
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_026.md:2:20:2:21
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_026.md:2:21:2:22
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_026.md:2:22:2:23
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_026.md:2:23:2:24
PARSE ERROR - fuzz_crash_026.md:2:26:2:27
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_026.md:2:27:2:29
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_026.md:2:29:2:29
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_026.md:8:1:8:4
PARSE ERROR - fuzz_crash_026.md:10:5:10:8
PARSE ERROR - fuzz_crash_026.md:13:5:13:6
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_026.md:13:42:13:43
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_026.md:15:7:15:8
INVALID STATEMENT - fuzz_crash_026.md:2:13:2:14
INVALID STATEMENT - fuzz_crash_026.md:2:17:2:18
INVALID STATEMENT - fuzz_crash_026.md:2:18:2:19
INVALID STATEMENT - fuzz_crash_026.md:2:19:2:20
INVALID STATEMENT - fuzz_crash_026.md:2:20:2:21
INVALID STATEMENT - fuzz_crash_026.md:2:21:2:22
INVALID STATEMENT - fuzz_crash_026.md:2:22:2:23
INVALID STATEMENT - fuzz_crash_026.md:2:23:2:24
INVALID STATEMENT - fuzz_crash_026.md:2:27:2:29
INVALID STATEMENT - fuzz_crash_026.md:2:29:2:29
INVALID STATEMENT - fuzz_crash_026.md:8:1:8:4
MALFORMED TYPE - fuzz_crash_026.md:13:5:13:6
INVALID IF BRANCH - :0:0:0:0
INVALID STATEMENT - fuzz_crash_026.md:15:1:15:3
INVALID STATEMENT - fuzz_crash_026.md:15:5:15:8
INVALID STATEMENT - fuzz_crash_026.md:15:9:15:25
# PROBLEMS
**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.

**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.

**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.

**UNCLOSED STRING**
This string is missing a closing quote.

**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.

**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.

**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.

**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.

**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.

**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.

**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.

**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.

**PARSE ERROR**
A parsing error occurred: `expected_package_or_platform_colon`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_026.md:1:1:1:4:**
```roc
app
```
^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **\** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_026.md:2:13:2:14:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
            ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **@** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_026.md:2:17:2:18:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
                ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **\** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_026.md:2:18:2:19:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
                 ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **\** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_026.md:2:19:2:20:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
                  ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **-** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_026.md:2:20:2:21:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
                   ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **\** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_026.md:2:21:2:22:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
                    ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **\** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_026.md:2:22:2:23:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
                     ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **\** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_026.md:2:23:2:24:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
                      ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

Here is the problematic code:
**fuzz_crash_026.md:2:26:2:27:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
                         ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token ** }** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_026.md:2:27:2:29:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
                          ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_026.md:2:29:2:29:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
                            


**UNEXPECTED TOKEN IN EXPRESSION**
The token **9el** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_026.md:8:1:8:4:**
```roc
9el
```
^^^


**PARSE ERROR**
A parsing error occurred: `expected_type_field_name`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_026.md:10:5:10:8:**
```roc
    Res -> Str
```
    ^^^


**PARSE ERROR**
A parsing error occurred: `expected_ty_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_026.md:13:5:13:6:**
```roc
 ,  ]er = |id| if (id > 10) "big" else   @
```
    ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **@** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_026.md:13:42:13:43:**
```roc
 ,  ]er = |id| if (id > 10) "big" else   @
```
                                         ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_026.md:15:7:15:8:**
```roc
ma  ! = |_|    ser(100)
```
      ^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_026.md:2:13:2:14:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
            ^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_026.md:2:17:2:18:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
                ^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_026.md:2:18:2:19:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
                 ^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_026.md:2:19:2:20:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
                  ^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_026.md:2:20:2:21:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
                   ^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_026.md:2:21:2:22:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
                    ^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_026.md:2:22:2:23:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
                     ^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_026.md:2:23:2:24:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
                      ^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_026.md:2:27:2:29:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
                          ^^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_026.md:2:29:2:29:**
```roc
[main!] { p\\   @\\-\\\Fc" }
```
                            


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_026.md:8:1:8:4:**
```roc
9el
```
^^^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**fuzz_crash_026.md:13:5:13:6:**
```roc
 ,  ]er = |id| if (id > 10) "big" else   @
```
    ^


**INVALID IF BRANCH**
The `else` branch of this `if` expression could not be processed.

The `else` branch must contain a valid expression. Check for syntax errors or missing values.

Note: Every `if` expression in Roc must have an `else` branch, and both branches must have the same type.

**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_026.md:15:1:15:3:**
```roc
ma  ! = |_|    ser(100)
```
^^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_026.md:15:5:15:8:**
```roc
ma  ! = |_|    ser(100)
```
    ^^^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_026.md:15:9:15:25:**
```roc
ma  ! = |_|    ser(100)
```
        ^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),
OpenSquare(2:1-2:2),LowerIdent(2:2-2:7),CloseSquare(2:7-2:8),OpenCurly(2:9-2:10),LowerIdent(2:11-2:12),OpBackslash(2:12-2:13),OpBackslash(2:13-2:14),MalformedOpaqueNameWithoutName(2:17-2:18),OpBackslash(2:18-2:19),OpBackslash(2:19-2:20),OpBinaryMinus(2:20-2:21),OpBackslash(2:21-2:22),OpBackslash(2:22-2:23),OpBackslash(2:23-2:24),UpperIdent(2:24-2:26),StringStart(2:26-2:27),StringPart(2:27-2:29),StringEnd(2:29-2:29),
UpperIdent(4:1-4:7),OpColon(4:8-4:9),UpperIdent(4:10-4:13),
LowerIdent(6:1-6:4),OpColon(6:5-6:6),UpperIdent(6:7-6:13),
MalformedNumberNoExponentDigits(8:1-8:4),
LowerIdent(9:1-9:3),OpColon(9:3-9:4),OpenCurly(9:5-9:6),
UpperIdent(10:5-10:8),OpArrow(10:9-10:11),UpperIdent(10:12-10:15),
LowerIdent(11:1-11:6),OpAssign(11:6-11:7),
Comma(13:2-13:3),CloseSquare(13:5-13:6),LowerIdent(13:6-13:8),OpAssign(13:9-13:10),OpBar(13:11-13:12),LowerIdent(13:12-13:14),OpBar(13:14-13:15),KwIf(13:16-13:18),OpenRound(13:19-13:20),LowerIdent(13:20-13:22),OpGreaterThan(13:23-13:24),Int(13:25-13:27),CloseRound(13:27-13:28),StringStart(13:29-13:30),StringPart(13:30-13:33),StringEnd(13:33-13:34),KwElse(13:35-13:39),MalformedOpaqueNameWithoutName(13:42-13:43),
LowerIdent(15:1-15:3),OpBang(15:5-15:6),OpAssign(15:7-15:8),OpBar(15:9-15:10),Underscore(15:10-15:11),OpBar(15:11-15:12),LowerIdent(15:17-15:20),NoSpaceOpenRound(15:20-15:21),Int(15:21-15:24),CloseRound(15:24-15:25),EndOfFile(15:25-15:25),
~~~
# PARSE
~~~clojure
(file @1.1-15.25
	(malformed-header @1.1-2.13 (tag "expected_package_or_platform_colon"))
	(statements
		(e-malformed @2.13-2.14 (reason "expr_unexpected_token"))
		(e-malformed @2.17-2.18 (reason "expr_unexpected_token"))
		(e-malformed @2.18-2.19 (reason "expr_unexpected_token"))
		(e-malformed @2.19-2.20 (reason "expr_unexpected_token"))
		(e-malformed @2.20-2.21 (reason "expr_unexpected_token"))
		(e-malformed @2.21-2.22 (reason "expr_unexpected_token"))
		(e-malformed @2.22-2.23 (reason "expr_unexpected_token"))
		(e-malformed @2.23-2.24 (reason "expr_unexpected_token"))
		(s-malformed @2.24-2.27 (tag "expected_colon_after_type_annotation"))
		(e-malformed @2.27-2.29 (reason "expr_unexpected_token"))
		(e-malformed @2.29-2.29 (reason "expr_unexpected_token"))
		(s-type-decl @4.1-4.13
			(header @4.1-4.7 (name "UserId")
				(args))
			(ty @4.10-4.13 (name "U64")))
		(s-type-anno @6.1-6.13 (name "ser")
			(ty @6.7-6.13 (name "UserId")))
		(e-malformed @8.1-8.4 (reason "expr_unexpected_token"))
		(s-type-anno @9.1-13.6 (name "va")
			(ty-malformed @13.5-13.6 (tag "expected_ty_close_curly_or_comma")))
		(s-decl @13.6-13.43
			(p-ident @13.6-13.8 (raw "er"))
			(e-lambda @13.11-13.43
				(args
					(p-ident @13.12-13.14 (raw "id")))
				(e-if-then-else @13.16-13.43
					(e-tuple @13.19-13.28
						(e-binop @13.20-13.27 (op ">")
							(e-ident @13.20-13.22 (raw "id"))
							(e-int @13.25-13.27 (raw "10"))))
					(e-string @13.29-13.34
						(e-string-part @13.30-13.33 (raw "big")))
					(e-malformed @13.42-13.43 (reason "expr_unexpected_token")))))
		(e-ident @15.1-15.3 (raw "ma"))
		(unary "!"
			(e-malformed @15.7-15.8 (reason "expr_unexpected_token")))
		(e-lambda @15.9-15.25
			(args
				(p-underscore))
			(e-apply @15.17-15.25
				(e-ident @15.17-15.20 (raw "ser"))
				(e-int @15.21-15.24 (raw "100"))))))
~~~
# FORMATTED
~~~roc


UserId : U64

ser : UserId


va : 
er = |id| if (id > 10) "big" else 

ma
!
|_| ser(100)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @13.6-13.8 (ident "er"))
		(e-lambda @13.11-13.43
			(args
				(p-assign @13.12-13.14 (ident "id")))
			(e-runtime-error (tag "if_else_not_canonicalized"))))
	(s-alias-decl @4.1-4.13
		(ty-header @4.1-4.7 (name "UserId"))
		(ty @4.10-4.13 (name "U64"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @13.6-13.8 (type "_arg -> Error")))
	(type_decls
		(alias @4.1-4.13 (type "UserId")
			(ty-header @4.1-4.7 (name "UserId"))))
	(expressions
		(expr @13.11-13.43 (type "_arg -> Error"))))
~~~
