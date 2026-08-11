# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
= "te
~~~
# EXPECTED
UNCLOSED STRING - fuzz_crash_003.md:1:3:1:6
UNEXPECTED STATEMENT - fuzz_crash_003.md:1:1:1:2
UNEXPECTED STATEMENT - fuzz_crash_003.md:1:3:1:4
UNEXPECTED STATEMENT - fuzz_crash_003.md:1:4:1:6
UNEXPECTED STATEMENT - fuzz_crash_003.md:1:6:1:6
# PROBLEMS
── ✗ unclosed string ───────────────────────────────────── fuzz_crash_003.md:1:3

This string is missing a closing quote.

= "te
  ^^^

── ✗ unexpected statement ──────────────────────────────── fuzz_crash_003.md:1:1

I was parsing a statement, and this token cannot start a statement here.

= "te
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found = here.

── ✗ unexpected statement ──────────────────────────────── fuzz_crash_003.md:1:3

I was parsing a statement, and this token cannot start a statement here.

= "te
  ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found " here.

── ✗ unexpected statement ──────────────────────────────── fuzz_crash_003.md:1:4

I was parsing a statement, and this token cannot start a statement here.

= "te
   ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found te here.

── ✗ unexpected statement ──────────────────────────────── fuzz_crash_003.md:1:6

I was parsing a statement, and this token cannot start a statement here.

= "te
     ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I reached the end of the file before this construct was complete.

# TOKENS
~~~zig
OpAssign,StringStart,StringPart,StringEnd,
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
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
