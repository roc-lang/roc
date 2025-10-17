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
INVALID UNICODE ESCAPE SEQUENCE - :0:0:0:0
INVALID UNICODE ESCAPE SEQUENCE - :0:0:0:0
INVALID UNICODE ESCAPE SEQUENCE - :0:0:0:0
INVALID UNICODE ESCAPE SEQUENCE - :0:0:0:0
INVALID UNICODE ESCAPE SEQUENCE - :0:0:0:0
INVALID ESCAPE SEQUENCE - :0:0:0:0
UNCLOSED STRING - :0:0:0:0
PARSE ERROR - string.md:13:1:13:2
PARSE ERROR - string.md:13:2:13:3
PARSE ERROR - string.md:13:3:13:3
# PROBLEMS
**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

```roc
	"\u",
```
	 ^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

```roc
	"\u)",
```
	 ^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

```roc
	"\u(",
```
	 ^^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

```roc
	"\u()",
```
	 ^^^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

```roc
	"\u(K)",
```
	 ^^^^^


**INVALID ESCAPE SEQUENCE**
This escape sequence is not recognized.

```roc
"\

```


**UNCLOSED STRING**
This string is missing a closing quote.

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
		(patt (type "(Str, Str, Str, Str, Str, Str, Str, Str)")))
	(expressions
		(expr (type "(Str, Str, Str, Str, Str, Str, Str, Str)"))))
~~~
