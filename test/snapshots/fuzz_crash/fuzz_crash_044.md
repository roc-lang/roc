# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}{{0
}}

""
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_044.md:1:20:1:21
UNEXPECTED STATEMENT - fuzz_crash_044.md:1:21:1:22
UNEXPECTED STATEMENT - fuzz_crash_044.md:1:22:1:23
UNEXPECTED STATEMENT - fuzz_crash_044.md:2:1:2:2
UNEXPECTED STATEMENT - fuzz_crash_044.md:2:2:2:3
UNEXPECTED STATEMENT - fuzz_crash_044.md:4:1:4:2
UNEXPECTED STATEMENT - fuzz_crash_044.md:4:2:4:2
UNEXPECTED STATEMENT - fuzz_crash_044.md:4:2:4:3
# PROBLEMS
── ✗ unexpected statement ─────────────────────────────── fuzz_crash_044.md:1:20

I was parsing a statement, and this token cannot start a statement here.

app[]{f:platform""}{{0
                   ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ─────────────────────────────── fuzz_crash_044.md:1:21

I was parsing a statement, and this token cannot start a statement here.

app[]{f:platform""}{{0
                    ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ─────────────────────────────── fuzz_crash_044.md:1:22

I was parsing a statement, and this token cannot start a statement here.

app[]{f:platform""}{{0
                     ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found 0 here.

── ✗ unexpected statement ──────────────────────────────── fuzz_crash_044.md:2:1

I was parsing a statement, and this token cannot start a statement here.

}}
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ──────────────────────────────── fuzz_crash_044.md:2:2

I was parsing a statement, and this token cannot start a statement here.

}}
 ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ──────────────────────────────── fuzz_crash_044.md:4:1

I was parsing a statement, and this token cannot start a statement here.

""
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found " here.

── ✗ unexpected statement ──────────────────────────────── fuzz_crash_044.md:4:2

I was parsing a statement, and this token cannot start a statement here.

""
 ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I reached the end of the file before this construct was complete.

── ✗ unexpected statement ──────────────────────────────── fuzz_crash_044.md:4:2

I was parsing a statement, and this token cannot start a statement here.

""
 ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found " here.

# TOKENS
~~~zig
KwApp,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,OpenCurly,OpenCurly,Int,
CloseCurly,CloseCurly,
StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides)
		(record-field (name "f")
			(e-string
				(e-string-part (raw ""))))
		(packages
			(record-field (name "f")
				(e-string
					(e-string-part (raw ""))))))
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
app [] { f: platform "" }



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
