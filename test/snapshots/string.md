# META
~~~ini
description=two strings
type=file:String.roc
~~~
# SOURCE
~~~roc
String := {}

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
PARSE ERROR - string.md:15:1:15:2
PARSE ERROR - string.md:15:2:15:3
PARSE ERROR - string.md:15:3:15:3
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

**string.md:15:1:15:2:**
```roc
"\
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**string.md:15:2:15:3:**
```roc
"\
```
 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**string.md:15:3:15:3:**
```roc
"\
```
  ^


# TOKENS
~~~zig
UpperIdent(1:1-1:7),OpColonEqual(1:8-1:10),OpenCurly(1:11-1:12),CloseCurly(1:12-1:13),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),OpenRound(3:5-3:6),
StringStart(4:2-4:3),StringPart(4:3-4:6),StringEnd(4:6-4:7),Comma(4:7-4:8),
StringStart(5:2-5:3),StringPart(5:3-5:6),StringEnd(5:6-5:7),Comma(5:7-5:8),
StringStart(6:2-6:3),MalformedStringPart(6:3-6:5),StringEnd(6:5-6:6),Comma(6:6-6:7),
StringStart(7:2-7:3),MalformedStringPart(7:3-7:6),StringEnd(7:6-7:7),Comma(7:7-7:8),
StringStart(8:2-8:3),MalformedStringPart(8:3-8:6),StringEnd(8:6-8:7),Comma(8:7-8:8),
StringStart(9:2-9:3),MalformedStringPart(9:3-9:7),StringEnd(9:7-9:8),Comma(9:8-9:9),
StringStart(10:2-10:3),MalformedStringPart(10:3-10:8),StringEnd(10:8-10:9),Comma(10:9-10:10),
StringStart(11:2-11:3),StringPart(11:3-11:12),StringEnd(11:12-11:13),Comma(11:13-11:14),
CloseRound(12:1-12:2),
StringStart(15:1-15:2),MalformedStringPart(15:2-15:3),StringEnd(15:3-15:3),
EndOfFile(16:1-16:1),
~~~
# PARSE
~~~clojure
(file @1.1-15.3
	(type-module @1.1-1.7)
	(statements
		(s-type-decl @1.1-1.13
			(header @1.1-1.7 (name "String")
				(args))
			(ty-record @1.11-1.13))
		(s-decl @3.1-12.2
			(p-ident @3.1-3.2 (raw "x"))
			(e-tuple @3.5-12.2
				(e-string @4.2-4.7
					(e-string-part @4.3-4.6 (raw "one")))
				(e-string @5.2-5.7
					(e-string-part @5.3-5.6 (raw "two")))
				(e-string @6.2-6.6)
				(e-string @7.2-7.7)
				(e-string @8.2-8.7)
				(e-string @9.2-9.8)
				(e-string @10.2-10.9)
				(e-string @11.2-11.13
					(e-string-part @11.3-11.12 (raw "\u(1F680)")))))
		(s-malformed @15.1-15.2 (tag "statement_unexpected_token"))
		(s-malformed @15.2-15.3 (tag "statement_unexpected_token"))
		(s-malformed @15.3-15.3 (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
String := {}

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
		(p-assign @3.1-3.2 (ident "x"))
		(e-tuple @3.5-12.2
			(elems
				(e-string @4.2-4.7
					(e-literal @4.3-4.6 (string "one")))
				(e-string @5.2-5.7
					(e-literal @5.3-5.6 (string "two")))
				(e-string @6.2-6.6)
				(e-string @7.2-7.7)
				(e-string @8.2-8.7)
				(e-string @9.2-9.8)
				(e-string @10.2-10.9)
				(e-string @11.2-11.13
					(e-literal @11.3-11.12 (string "\u(1F680)"))))))
	(s-nominal-decl @1.1-1.13
		(ty-header @1.1-1.7 (name "String"))
		(ty-record @1.11-1.13)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.2 (type "(Str, Str, Str, Str, Str, Str, Str, Str)")))
	(type_decls
		(nominal @1.1-1.13 (type "String")
			(ty-header @1.1-1.7 (name "String"))))
	(expressions
		(expr @3.5-12.2 (type "(Str, Str, Str, Str, Str, Str, Str, Str)"))))
~~~
