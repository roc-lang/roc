# META
~~~ini
description=canonicalize hang: malformed numeric annotation declarations
type=file
source_escapes=true
~~~
# SOURCE
~~~roc
a:F
a=0
b:F
b=G.70000c:c=0
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_hang_003.md:4:4:4:11
UNEXPECTED STATEMENT - fuzz_hang_003.md:4:11:4:12
UNDECLARED TYPE - fuzz_hang_003.md:1:3:1:4
UNDECLARED TYPE - fuzz_hang_003.md:3:3:3:4
# PROBLEMS
── ✗ unexpected statement ───────────────────────────────── fuzz_hang_003.md:4:4

I was parsing a statement, and this token cannot start a statement here.

b=G.70000c:c=0
   ^^^^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found .70000c here.

── ✗ unexpected statement ──────────────────────────────── fuzz_hang_003.md:4:11

I was parsing a statement, and this token cannot start a statement here.

b=G.70000c:c=0
          ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found : here.

── ✗ undeclared type ────────────────────────────────────── fuzz_hang_003.md:1:3

The type F is not declared in this scope.

a:F
  ^

── ✗ undeclared type ────────────────────────────────────── fuzz_hang_003.md:3:3

The type F is not declared in this scope.

b:F
  ^

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,MalformedNumberBadSuffix,OpColon,LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "a")
			(ty (name "F")))
		(s-decl
			(p-ident (raw "a"))
			(e-int (raw "0")))
		(s-type-anno (name "b")
			(ty (name "F")))
		(s-decl
			(p-ident (raw "b"))
			(e-tag (raw "G")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "c"))
			(e-int (raw "0")))))
~~~
# FORMATTED
~~~roc
a : F
a = 0

b : F
b = G
c = 0
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-num (value "0"))
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "b"))
		(e-tag (name "G"))
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "c"))
		(e-num (value "0"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Dec")))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Dec"))))
~~~
