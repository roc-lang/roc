# META
~~~ini
description=Match expression with f64 literal pattern (should error)
type=expr
~~~
# SOURCE
~~~roc
match x {
    3.14f64 => "pi"
    0.0f64 => "zero"
    value => "other"
}
~~~
# EXPECTED
UNEXPECTED TOKEN IN PATTERN - f64_pattern_literal_error.md:2:5:2:15
UNEXPECTED TOKEN IN PATTERN - f64_pattern_literal_error.md:3:5:3:14
UNDEFINED VARIABLE - f64_pattern_literal_error.md:1:7:1:8
UNUSED VARIABLE - f64_pattern_literal_error.md:4:5:4:10
# PROBLEMS
**UNEXPECTED TOKEN IN PATTERN**
The token **3.14f64 =>** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**f64_pattern_literal_error.md:2:5:2:15:**
```roc
    3.14f64 => "pi"
```
    ^^^^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **0.0f64 =>** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**f64_pattern_literal_error.md:3:5:3:14:**
```roc
    0.0f64 => "zero"
```
    ^^^^^^^^^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:8),OpenCurly(1:9-1:10),Newline(1:1-1:1),
MalformedNumberBadSuffix(2:5-2:12),OpFatArrow(2:13-2:15),StringStart(2:16-2:17),StringPart(2:17-2:19),StringEnd(2:19-2:20),Newline(1:1-1:1),
MalformedNumberBadSuffix(3:5-3:11),OpFatArrow(3:12-3:14),StringStart(3:15-3:16),StringPart(3:16-3:20),StringEnd(3:20-3:21),Newline(1:1-1:1),
LowerIdent(4:5-4:10),OpFatArrow(4:11-4:13),StringStart(4:14-4:15),StringPart(4:15-4:20),StringEnd(4:20-4:21),Newline(1:1-1:1),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.8 (raw "x"))
	(branches
		(branch @1.1-1.1
			(p-malformed @2.5-2.15 (tag "pattern_unexpected_token"))
			(e-string @2.16-2.20
				(e-string-part @2.17-2.19 (raw "pi"))))
		(branch @1.1-1.1
			(p-malformed @3.5-3.14 (tag "pattern_unexpected_token"))
			(e-string @3.15-3.21
				(e-string-part @3.16-3.20 (raw "zero"))))
		(branch @1.1-1.1
			(p-ident @4.5-4.10 (raw "value"))
			(e-string @4.14-4.21
				(e-string-part @4.15-4.20 (raw "other"))))))
~~~
# FORMATTED
~~~roc
match x {
	 => "pi"
	 => "zero"
	value => "other"
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-5.2
	(match @1.1-5.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(p-runtime-error @2.5-2.15 (tag "pattern_not_canonicalized") (degenerate false)))
				(value
					(e-string @2.16-2.20
						(e-literal @2.17-2.19 (string "pi")))))
			(branch
				(patterns
					(p-runtime-error @3.5-3.14 (tag "pattern_not_canonicalized") (degenerate false)))
				(value
					(e-string @3.15-3.21
						(e-literal @3.16-3.20 (string "zero")))))
			(branch
				(patterns
					(p-assign @4.5-4.10 (ident "value") (degenerate false)))
				(value
					(e-string @4.14-4.21
						(e-literal @4.15-4.20 (string "other"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "Str"))
~~~
