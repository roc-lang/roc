# META
~~~ini
description=package_header_nonempty_multiline (6)
type=file
~~~
# SOURCE
~~~roc
package # Comment after keyword
	[ # Comment after exposes open
		something, # Comment after exposed item
		SomeType, # Comment after last exposed item
	]
	{ # Comment after packages open
		somePkg: "../main.roc", # Comment after package
		other: "../../other/main.roc", # Comment after last package
	}
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - package_header_nonempty_multiline_6.md:10:2:10:4
UNEXPECTED TOKEN IN EXPRESSION - package_header_nonempty_multiline_6.md:10:3:10:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**package_header_nonempty_multiline_6.md:10:2:10:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**package_header_nonempty_multiline_6.md:10:3:10:4:**
```roc
~~~
```
  ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
KwPackage(1:1-1:8),Newline(1:10-1:32),
OpenSquare(2:2-2:3),Newline(2:5-2:32),
LowerIdent(3:3-3:12),Comma(3:12-3:13),Newline(3:15-3:42),
UpperIdent(4:3-4:11),Comma(4:11-4:12),Newline(4:14-4:46),
CloseSquare(5:2-5:3),Newline(1:1-1:1),
OpenCurly(6:2-6:3),Newline(6:5-6:33),
LowerIdent(7:3-7:10),OpColon(7:10-7:11),StringStart(7:12-7:13),StringPart(7:13-7:24),StringEnd(7:24-7:25),Comma(7:25-7:26),Newline(7:28-7:50),
LowerIdent(8:3-8:8),OpColon(8:8-8:9),StringStart(8:10-8:11),StringPart(8:11-8:31),StringEnd(8:31-8:32),Comma(8:32-8:33),Newline(8:35-8:62),
CloseCurly(9:2-9:3),Newline(1:1-1:1),
MalformedUnknownToken(10:1-10:2),MalformedUnknownToken(10:2-10:3),MalformedUnknownToken(10:3-10:4),EndOfFile(10:4-10:4),
~~~
# PARSE
~~~clojure
(file @1.1-10.4
	(package @1.1-9.3
		(exposes @2.2-5.3
			(exposed-lower-ident (text "something"))
			(exposed-upper-ident (text "SomeType")))
		(packages @6.2-9.3
			(record-field @7.3-7.26 (name "somePkg")
				(e-string @7.12-7.25
					(e-string-part @7.13-7.24 (raw "../main.roc"))))
			(record-field @8.3-8.33 (name "other")
				(e-string @8.10-8.32
					(e-string-part @8.11-8.31 (raw "../../other/main.roc"))))))
	(statements
		(e-malformed @10.2-10.4 (reason "expr_unexpected_token"))
		(e-malformed @10.3-10.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
package # Comment after keyword
	[ # Comment after exposes open
		something, # Comment after exposed item
		SomeType, # Comment after last exposed item
	]
	{ # Comment after packages open
		somePkg: "../main.roc", # Comment after package
		other: "../../other/main.roc", # Comment after last package
	}
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
