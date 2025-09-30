# META
~~~ini
description=two strings
type=file
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
MISSING MAIN! FUNCTION - string.md:1:1:13:3
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


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**string.md:1:1:13:3:**
```roc
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
```


# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:3-1:4),OpenRound(1:5-1:6),
StringStart(2:2-2:3),StringPart(2:3-2:6),StringEnd(2:6-2:7),Comma(2:7-2:8),
StringStart(3:2-3:3),StringPart(3:3-3:6),StringEnd(3:6-3:7),Comma(3:7-3:8),
StringStart(4:2-4:3),MalformedStringPart(4:3-4:5),StringEnd(4:5-4:6),Comma(4:6-4:7),
StringStart(5:2-5:3),MalformedStringPart(5:3-5:6),StringEnd(5:6-5:7),Comma(5:7-5:8),
StringStart(6:2-6:3),MalformedStringPart(6:3-6:6),StringEnd(6:6-6:7),Comma(6:7-6:8),
StringStart(7:2-7:3),MalformedStringPart(7:3-7:7),StringEnd(7:7-7:8),Comma(7:8-7:9),
StringStart(8:2-8:3),MalformedStringPart(8:3-8:8),StringEnd(8:8-8:9),Comma(8:9-8:10),
StringStart(9:2-9:3),StringPart(9:3-9:12),StringEnd(9:12-9:13),Comma(9:13-9:14),
CloseRound(10:1-10:2),
StringStart(13:1-13:2),MalformedStringPart(13:2-13:3),StringEnd(13:3-13:3),
EndOfFile(14:1-14:1),
~~~
# PARSE
~~~clojure
(file @1.1-13.3
	(type-module @1.1-1.2)
	(statements
		(s-decl @1.1-10.2
			(p-ident @1.1-1.2 (raw "x"))
			(e-tuple @1.5-10.2
				(e-string @2.2-2.7
					(e-string-part @2.3-2.6 (raw "one")))
				(e-string @3.2-3.7
					(e-string-part @3.3-3.6 (raw "two")))
				(e-string @4.2-4.6)
				(e-string @5.2-5.7)
				(e-string @6.2-6.7)
				(e-string @7.2-7.8)
				(e-string @8.2-8.9)
				(e-string @9.2-9.13
					(e-string-part @9.3-9.12 (raw "\u(1F680)")))))
		(s-malformed @13.1-13.2 (tag "statement_unexpected_token"))
		(s-malformed @13.2-13.3 (tag "statement_unexpected_token"))
		(s-malformed @13.3-13.3 (tag "statement_unexpected_token"))))
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
		(p-assign @1.1-1.2 (ident "x"))
		(e-tuple @1.5-10.2
			(elems
				(e-string @2.2-2.7
					(e-literal @2.3-2.6 (string "one")))
				(e-string @3.2-3.7
					(e-literal @3.3-3.6 (string "two")))
				(e-string @4.2-4.6)
				(e-string @5.2-5.7)
				(e-string @6.2-6.7)
				(e-string @7.2-7.8)
				(e-string @8.2-8.9)
				(e-string @9.2-9.13
					(e-literal @9.3-9.12 (string "\u(1F680)")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.2 (type "(Str, Str, Str, Str, Str, Str, Str, Str)")))
	(expressions
		(expr @1.5-10.2 (type "(Str, Str, Str, Str, Str, Str, Str, Str)"))))
~~~
