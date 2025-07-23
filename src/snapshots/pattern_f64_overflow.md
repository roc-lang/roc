# META
~~~ini
description=Match expression with fractional literals that exceed Dec precision
type=expr
~~~
# SOURCE
~~~roc
match x {
    1e100 => "very large number"
    1e-40 => "very small number"
    1.7976931348623157e308 => "near f64 max"
    0.0 => "zero"
    value => "other"
}
~~~
# EXPECTED
UNDEFINED VARIABLE - pattern_f64_overflow.md:1:7:1:8
COMPILER DIAGNOSTIC - pattern_f64_overflow.md:0:0:0:0
COMPILER DIAGNOSTIC - pattern_f64_overflow.md:0:0:0:0
COMPILER DIAGNOSTIC - pattern_f64_overflow.md:0:0:0:0
UNUSED VARIABLE - pattern_f64_overflow.md:6:5:6:10
# PROBLEMS
**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'x' is not defined:
**pattern_f64_overflow.md:1:7:1:8:**
```roc
match x {
```
      ^


**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'f64_pattern_literal' is not yet handled in report generation.
**pattern_f64_overflow.md:0:0:0:0**

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'f64_pattern_literal' is not yet handled in report generation.
**pattern_f64_overflow.md:0:0:0:0**

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'f64_pattern_literal' is not yet handled in report generation.
**pattern_f64_overflow.md:0:0:0:0**

**UNUSED VARIABLE**

**Unused Variable**
The variable 'value' is defined but never used:
**pattern_f64_overflow.md:6:5:6:10:**
```roc
    value => "other"
```
    ^^^^^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:8),OpenCurly(1:9-1:10),
Float(2:5-2:10),OpFatArrow(2:11-2:13),StringStart(2:14-2:15),StringPart(2:15-2:32),StringEnd(2:32-2:33),
Float(3:5-3:10),OpFatArrow(3:11-3:13),StringStart(3:14-3:15),StringPart(3:15-3:32),StringEnd(3:32-3:33),
Float(4:5-4:27),OpFatArrow(4:28-4:30),StringStart(4:31-4:32),StringPart(4:32-4:44),StringEnd(4:44-4:45),
Float(5:5-5:8),OpFatArrow(5:9-5:11),StringStart(5:12-5:13),StringPart(5:13-5:17),StringEnd(5:17-5:18),
LowerIdent(6:5-6:10),OpFatArrow(6:11-6:13),StringStart(6:14-6:15),StringPart(6:15-6:20),StringEnd(6:20-6:21),
CloseCurly(7:1-7:2),EndOfFile(7:2-7:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.8 (raw "x"))
	(branches
		(branch @2.5-2.33
			(p-frac @2.5-2.10 (raw "1e100"))
			(e-string @2.14-2.33
				(e-string-part @2.15-2.32 (raw "very large number"))))
		(branch @3.5-3.33
			(p-frac @3.5-3.10 (raw "1e-40"))
			(e-string @3.14-3.33
				(e-string-part @3.15-3.32 (raw "very small number"))))
		(branch @4.5-4.45
			(p-frac @4.5-4.27 (raw "1.7976931348623157e308"))
			(e-string @4.31-4.45
				(e-string-part @4.32-4.44 (raw "near f64 max"))))
		(branch @5.5-5.18
			(p-frac @5.5-5.8 (raw "0.0"))
			(e-string @5.12-5.18
				(e-string-part @5.13-5.17 (raw "zero"))))
		(branch @6.5-6.21
			(p-ident @6.5-6.10 (raw "value"))
			(e-string @6.14-6.21
				(e-string-part @6.15-6.20 (raw "other"))))))
~~~
# FORMATTED
~~~roc
match x {
	1e100 => "very large number"
	1e-40 => "very small number"
	1.7976931348623157e308 => "near f64 max"
	0.0 => "zero"
	value => "other"
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-7.2
	(match @1.1-7.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "f64_pattern_literal"))))
				(value
					(e-string @2.14-2.33
						(e-literal @2.15-2.32 (string "very large number")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "f64_pattern_literal"))))
				(value
					(e-string @3.14-3.33
						(e-literal @3.15-3.32 (string "very small number")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "f64_pattern_literal"))))
				(value
					(e-string @4.31-4.45
						(e-literal @4.32-4.44 (string "near f64 max")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-small-dec @5.5-5.8)))
				(value
					(e-string @5.12-5.18
						(e-literal @5.13-5.17 (string "zero")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-assign @6.5-6.10 (ident "value"))))
				(value
					(e-string @6.14-6.21
						(e-literal @6.15-6.20 (string "other"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-7.2 (type "Str"))
~~~
