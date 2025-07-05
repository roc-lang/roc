# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
H{o,
    ]
foo =

    "on        (string 'onmo %')))
~~~
~~~
# EXPECTED
ASCII CONTROL CHARACTER - fuzz_crash_010.md:1:1:1:3
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_010.md:6:1:6:3
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_010.md:6:2:6:4
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_010.md:6:3:6:4
# PROBLEMS
**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.

**MISMATCHED BRACE**
This brace does not match the corresponding opening brace.

**UNCLOSED STRING**
This string is missing a closing quote.

**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_010.md:1:1:1:3:**
```roc
H{o,
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_010.md:6:1:6:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_010.md:6:2:6:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_010.md:6:3:6:4:**
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

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:3-1:4),Comma(1:4-1:5),Newline(1:1-1:1),
CloseCurly(2:6-2:7),Newline(1:1-1:1),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),Newline(1:1-1:1),
Newline(1:1-1:1),
StringStart(5:5-5:6),StringPart(5:6-5:35),StringEnd(5:35-5:35),Newline(1:1-1:1),
MalformedUnknownToken(6:1-6:2),MalformedUnknownToken(6:2-6:3),MalformedUnknownToken(6:3-6:4),EndOfFile(6:4-6:4),
~~~
# PARSE
~~~clojure
(file @1.1-6.4
	(malformed-header @1.1-1.3 (tag "missing_header"))
	(statements
		(e-record @1.2-2.7
			(field (field "o") (optional false)))
		(s-decl @3.1-5.35
			(p-ident @3.1-3.4 (raw "foo"))
			(e-string @5.5-5.35
				(e-string-part @5.6-5.35 (raw "on        (string 'onmo %')))"))))
		(e-malformed @6.1-6.3 (reason "expr_unexpected_token"))
		(e-malformed @6.2-6.4 (reason "expr_unexpected_token"))
		(e-malformed @6.3-6.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
{
	o

}
foo = 

	"on        (string 'onmo %')))"

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.4 (ident "foo"))
		(e-string @5.5-5.35
			(e-literal @5.6-5.35 (string "on        (string 'onmo %')))")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.4 (type "Str")))
	(expressions
		(expr @5.5-5.35 (type "Str"))))
~~~
