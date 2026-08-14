# META
~~~ini
description=fuzz crash, unterminated single quote
type=file
~~~
# SOURCE
~~~roc
mule []

#el
vavar t= '
~~~
# EXPECTED
UNCLOSED SINGLE QUOTE - fuzz_crash_031.md:4:10:4:11
UNEXPECTED STATEMENT - fuzz_crash_031.md:1:1:1:5
UNEXPECTED STATEMENT - fuzz_crash_031.md:1:6:1:7
UNEXPECTED STATEMENT - fuzz_crash_031.md:1:7:1:8
UNEXPECTED STATEMENT - fuzz_crash_031.md:4:1:4:6
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_031.md:4:10:4:11
UNRECOGNIZED SYNTAX - fuzz_crash_031.md:4:10:4:11
# PROBLEMS
── ✗ unclosed single quote ────────────────────────────── fuzz_crash_031.md:4:10

This single-quoted literal is missing a closing quote.

vavar t= '
         ^

── ✗ unexpected statement ──────────────────────────────── fuzz_crash_031.md:1:1

I was parsing a statement, and this token cannot start a statement here.

mule []
^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found mule here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ──────────────────────────────── fuzz_crash_031.md:1:6

I was parsing a statement, and this token cannot start a statement here.

mule []
     ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ unexpected statement ──────────────────────────────── fuzz_crash_031.md:1:7

I was parsing a statement, and this token cannot start a statement here.

mule []
      ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ──────────────────────────────── fuzz_crash_031.md:4:1

I was parsing a statement, and this token cannot start a statement here.

vavar t= '
^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found vavar here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected expression syntax ─────────────────────── fuzz_crash_031.md:4:10

I was parsing an expression, and this token cannot start an expression here.

vavar t= '
         ^

Expressions can be names, literals, tags, records, lists, tuples, lambdas,
blocks, conditionals, matches, or function calls.

For example:
    add(1, 2)

I found ' here.

── ✗ unrecognized syntax ──────────────────────────────── fuzz_crash_031.md:4:10

I don't recognize this syntax.

vavar t= '
         ^

This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
LowerIdent,OpenSquare,CloseSquare,
LowerIdent,LowerIdent,OpAssign,MalformedSingleQuote,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "t"))
			(e-malformed (reason "expr_unexpected_token")))))
~~~
# FORMATTED
~~~roc


# el
t =
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "t"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
