# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}import	B	as
G	if 0{}else||0
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:3:2:5
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:6:2:7
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:7:2:8
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:8:2:9
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:9:2:13
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:13:2:14
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:14:2:15
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:15:2:16
MOD NOT FOUND - fuzz_crash_059.md:1:20:2:2
# PROBLEMS
── ✗ unexpected statement ──────────────────────────────── fuzz_crash_059.md:2:3

I was parsing a statement, and this token cannot start a statement here.

G if 0{}else||0
  ^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found if here.
That word is reserved by Roc, so it cannot be used as a name in this position.

── ✗ unexpected statement ──────────────────────────────── fuzz_crash_059.md:2:6

I was parsing a statement, and this token cannot start a statement here.

G if 0{}else||0
     ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found 0 here.

── ✗ unexpected statement ──────────────────────────────── fuzz_crash_059.md:2:7

I was parsing a statement, and this token cannot start a statement here.

G if 0{}else||0
      ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found { here.

── ✗ unexpected statement ──────────────────────────────── fuzz_crash_059.md:2:8

I was parsing a statement, and this token cannot start a statement here.

G if 0{}else||0
       ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found } here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ unexpected statement ──────────────────────────────── fuzz_crash_059.md:2:9

I was parsing a statement, and this token cannot start a statement here.

G if 0{}else||0
        ^^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found else here.
That word is reserved by Roc, so it cannot be used as a name in this position.

── ✗ unexpected statement ─────────────────────────────── fuzz_crash_059.md:2:13

I was parsing a statement, and this token cannot start a statement here.

G if 0{}else||0
            ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found | here.

── ✗ unexpected statement ─────────────────────────────── fuzz_crash_059.md:2:14

I was parsing a statement, and this token cannot start a statement here.

G if 0{}else||0
             ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found | here.

── ✗ unexpected statement ─────────────────────────────── fuzz_crash_059.md:2:15

I was parsing a statement, and this token cannot start a statement here.

G if 0{}else||0
              ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found 0 here.

── ✗ mod not found ─────────────────────────────────── fuzz_crash_059.md:1:20

The mod B was not found in this Roc project.

app[]{f:platform""}import B as
G if 0{}else||0

# TOKENS
~~~zig
KwApp,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,KwImport,UpperIdent,KwAs,
UpperIdent,KwIf,Int,OpenCurly,CloseCurly,KwElse,OpBar,OpBar,Int,
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
		(s-import (raw "B") (alias "G"))
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
import B as G
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import (mod "B")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
