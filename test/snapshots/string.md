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
PARSE ERROR - string.md:13:1:13:2
PARSE ERROR - string.md:13:2:13:3
PARSE ERROR - string.md:13:3:13:3
# PROBLEMS
**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

**string.md:4:3:4:5:**
```roc
	"\u",
```
	 ^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

**string.md:5:3:5:5:**
```roc
	"\u)",
```
	 ^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

**string.md:6:3:6:6:**
```roc
	"\u(",
```
	 ^^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

**string.md:7:3:7:7:**
```roc
	"\u()",
```
	 ^^^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

**string.md:8:3:8:8:**
```roc
	"\u(K)",
```
	 ^^^^^


**INVALID ESCAPE SEQUENCE**
This escape sequence is not recognized.

**string.md:13:2:14:1:**
```roc
"\

```


**UNCLOSED STRING**
This string is missing a closing quote.

**string.md:13:1:13:3:**
```roc
"\
```
^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**string.md:13:1:13:2:**
```roc
"\
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**string.md:13:2:13:3:**
```roc
"\
```
 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**string.md:13:3:13:3:**
```roc
"\
```
  ^


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
	(type-module)
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
					(e-literal (string "\u(1F680)")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(a, b, Str, Str, Str, Str, Str, c) where [a.try_from_str : Str -> Try(a, [InvalidStr(Str)]), b.try_from_str : Str -> Try(b, [InvalidStr(Str)]), c.try_from_str : Str -> Try(c, [InvalidStr(Str)])]")))
	(expressions
		(expr (type "(a, b, Str, Str, Str, Str, Str, c) where [a.try_from_str : Str -> Try(a, [InvalidStr(Str)]), b.try_from_str : Str -> Try(b, [InvalidStr(Str)]), c.try_from_str : Str -> Try(c, [InvalidStr(Str)])]"))))
~~~
