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
UNDEFINED VARIABLE - f64_pattern_literal_error.md:1:7:1:8
UNUSED VARIABLE - f64_pattern_literal_error.md:4:5:4:10
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**f64_pattern_literal_error.md:1:7:1:8:**
```roc
match x {
```
      ^


**UNUSED VARIABLE**
Variable `value` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_value` to suppress this warning.
The unused variable is declared here:
**f64_pattern_literal_error.md:4:5:4:10:**
```roc
    value => "other"
```
    ^^^^^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:8),OpenCurly(1:9-1:10),
Float(2:5-2:12),OpFatArrow(2:13-2:15),StringStart(2:16-2:17),StringPart(2:17-2:19),StringEnd(2:19-2:20),
Float(3:5-3:11),OpFatArrow(3:12-3:14),StringStart(3:15-3:16),StringPart(3:16-3:20),StringEnd(3:20-3:21),
LowerIdent(4:5-4:10),OpFatArrow(4:11-4:13),StringStart(4:14-4:15),StringPart(4:15-4:20),StringEnd(4:20-4:21),
CloseCurly(5:1-5:2),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.8 (raw "x"))
	(branches
		(branch @2.5-2.20
			(p-frac @2.5-2.12 (raw "3.14f64"))
			(e-string @2.16-2.20
				(e-string-part @2.17-2.19 (raw "pi"))))
		(branch @3.5-3.21
			(p-frac @3.5-3.11 (raw "0.0f64"))
			(e-string @3.15-3.21
				(e-string-part @3.16-3.20 (raw "zero"))))
		(branch @4.5-4.21
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
(e-match @1.1-5.2
	(match @1.1-5.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-frac-f64 @2.5-2.12 (value "3.14e0"))))
				(value
					(e-string @2.16-2.20
						(e-literal @2.17-2.19 (string "pi")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-frac-f64 @3.5-3.11 (value "0e0"))))
				(value
					(e-string @3.15-3.21
						(e-literal @3.16-3.20 (string "zero")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-assign @4.5-4.10 (ident "value"))))
				(value
					(e-string @4.14-4.21
						(e-literal @4.15-4.20 (string "other"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "Str"))
~~~
