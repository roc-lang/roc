# META
~~~ini
description=two strings
type=snippet
~~~
# SOURCE
~~~roc
x = (
	"one",
	"two",
	"\u",
	"\u)",
	"\u(",
	"\u()",
	"\u(K)",
	"\u(1F680)",
)

# Test backslash before EOF
"\
~~~
# EXPECTED
INVALID UNICODE ESCAPE SEQUENCE - string.md:4:3:4:5
INVALID UNICODE ESCAPE SEQUENCE - string.md:5:3:5:5
INVALID UNICODE ESCAPE SEQUENCE - string.md:6:3:6:6
INVALID UNICODE ESCAPE SEQUENCE - string.md:7:3:7:7
INVALID UNICODE ESCAPE SEQUENCE - string.md:8:3:8:8
INVALID ESCAPE SEQUENCE - string.md:13:2:14:1
UNCLOSED STRING - string.md:13:1:13:3
UNEXPECTED STATEMENT - string.md:13:1:13:2
UNEXPECTED STATEMENT - string.md:13:2:13:3
UNEXPECTED STATEMENT - string.md:13:3:13:3
# PROBLEMS

┌─────────────────────────────────┐
│ INVALID UNICODE ESCAPE SEQUENCE ├─ This Unicode escape sequence is not ─────┐
└┬────────────────────────────────┘  valid.                                   │
 │                                                                            │
 │  "\u",                                                                     │
 │   ‾‾                                                                       │
 └───────────────────────────────────────────────────────────── string.md:4:3 ┘



┌─────────────────────────────────┐
│ INVALID UNICODE ESCAPE SEQUENCE ├─ This Unicode escape sequence is not ─────┐
└┬────────────────────────────────┘  valid.                                   │
 │                                                                            │
 │  "\u)",                                                                    │
 │   ‾‾                                                                       │
 └───────────────────────────────────────────────────────────── string.md:5:3 ┘



┌─────────────────────────────────┐
│ INVALID UNICODE ESCAPE SEQUENCE ├─ This Unicode escape sequence is not ─────┐
└┬────────────────────────────────┘  valid.                                   │
 │                                                                            │
 │  "\u(",                                                                    │
 │   ‾‾‾                                                                      │
 └───────────────────────────────────────────────────────────── string.md:6:3 ┘



┌─────────────────────────────────┐
│ INVALID UNICODE ESCAPE SEQUENCE ├─ This Unicode escape sequence is not ─────┐
└┬────────────────────────────────┘  valid.                                   │
 │                                                                            │
 │  "\u()",                                                                   │
 │   ‾‾‾‾                                                                     │
 └───────────────────────────────────────────────────────────── string.md:7:3 ┘



┌─────────────────────────────────┐
│ INVALID UNICODE ESCAPE SEQUENCE ├─ This Unicode escape sequence is not ─────┐
└┬────────────────────────────────┘  valid.                                   │
 │                                                                            │
 │  "\u(K)",                                                                  │
 │   ‾‾‾‾‾                                                                    │
 └───────────────────────────────────────────────────────────── string.md:8:3 ┘



┌─────────────────────────┐
│ INVALID ESCAPE SEQUENCE ├─ This escape sequence is not recognized. ─────────┐
└┬────────────────────────┘                                                   │
 │                                                                            │
 │  "\                                                                        │
 │                                                                            │
 │                                                                            │
 └──────────────────────────────────────────────────────────── string.md:13:2 ┘



┌─────────────────┐
│ UNCLOSED STRING ├─ This string is missing a closing quote. ─────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  "\                                                                        │
 │  ‾‾                                                                        │
 └──────────────────────────────────────────────────────────── string.md:13:1 ┘



┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  "\                                                                        │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────────── string.md:13:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `"` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  "\                                                                        │
 │   ‾                                                                        │
 └──────────────────────────────────────────────────────────── string.md:13:2 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `\` here.

    Tip: Roc syntax does not use single backslashes. Roc lambda syntax is
    `|arg1, arg2| body`, and double backslash (`\\`) begins a line in a
    multiline string.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  "\                                                                        │
 │    ‾                                                                       │
 └──────────────────────────────────────────────────────────── string.md:13:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I reached the end of the file before this construct was complete.

# TOKENS
~~~zig
LowerIdent,OpAssign,OpenRound,
StringStart,StringPart,StringEnd,Comma,
StringStart,StringPart,StringEnd,Comma,
StringStart,MalformedStringPart,StringEnd,Comma,
StringStart,MalformedStringPart,StringEnd,Comma,
StringStart,MalformedStringPart,StringEnd,Comma,
StringStart,MalformedStringPart,StringEnd,Comma,
StringStart,MalformedStringPart,StringEnd,Comma,
StringStart,StringPart,StringEnd,Comma,
CloseRound,
StringStart,MalformedStringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-tuple
				(e-string
					(e-string-part (raw "one")))
				(e-string
					(e-string-part (raw "two")))
				(e-string)
				(e-string)
				(e-string)
				(e-string)
				(e-string)
				(e-string
					(e-string-part (raw "\u(1F680)")))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
x = (
	"one",
	"two",
	"",
	"",
	"",
	"",
	"",
	"\u(1F680)",
)

# Test backslash before EOF
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-tuple
			(elems
				(e-string
					(e-literal (string "one")))
				(e-string
					(e-literal (string "two")))
				(e-string)
				(e-string)
				(e-string)
				(e-string)
				(e-string)
				(e-string
					(e-literal (string "🚀")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(Str, Str, Str, Str, Str, Str, Str, Str)")))
	(expressions
		(expr (type "(Str, Str, Str, Str, Str, Str, Str, Str)"))))
~~~
