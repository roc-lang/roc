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
# PROBLEMS
**F64 NOT ALLOWED IN PATTERN**
This floating-point number cannot be used in a pattern match: 3.14f64

Floating-point numbers like F64 cannot be used in patterns because they don't have reliable equality comparison. Consider using a `Dec` (decimal) literal instead for exact matching, or use a guard condition with a range check.

For example, instead of:
3.14f64 => ...
Use:
3.14dec => ... for exact decimal matching.

**F64 NOT ALLOWED IN PATTERN**
This floating-point number cannot be used in a pattern match: 0.0f64

Floating-point numbers like F64 cannot be used in patterns because they don't have reliable equality comparison. Consider using a `Dec` (decimal) literal instead for exact matching, or use a guard condition with a range check.

For example, instead of:
0.0f64 => ...
Use:
0.0dec => ... for exact decimal matching.
# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:8),OpenCurly(1:9-1:10),Newline(1:1-1:1),
Frac(2:5-2:9),KwF64(2:9-2:12),OpFatArrow(2:13-2:15),StringStart(2:16-2:17),StringPart(2:17-2:19),StringEnd(2:19-2:20),Newline(1:1-1:1),
Frac(3:5-3:8),KwF64(3:8-3:11),OpFatArrow(3:12-3:14),StringStart(3:15-3:16),StringPart(3:16-3:20),StringEnd(3:20-3:21),Newline(1:1-1:1),
LowerIdent(4:5-4:10),OpFatArrow(4:11-4:13),StringStart(4:14-4:15),StringPart(4:15-4:20),StringEnd(4:20-4:21),Newline(1:1-1:1),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.8 (raw "x"))
	(branches
		(branch @2.5-3.5
			(p-frac @2.5-2.12 (raw "3.14f64"))
			(e-string @2.13-2.17
				(e-string-part @2.14-2.16 (raw "pi"))))
		(branch @1.1-1.1
			(p-frac @3.5-3.11 (raw "0.0f64"))
			(e-string @3.12-3.18
				(e-string-part @3.13-3.17 (raw "zero"))))
		(branch @4.5-5.1
			(p-ident @4.5-4.10 (raw "value"))
			(e-string @4.14-4.21
				(e-string-part @4.15-4.20 (raw "other"))))))
~~~
# FORMATTED
~~~roc
match x {
	3.14f64 => "pi"
	0.0f64 => "zero"
	value => "other"
}
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error @1.1-5.2
	(tag "f64_pattern_literal"))
~~~
# TYPES
NIL