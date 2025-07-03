# META
~~~ini
description=Fractional pattern literals are typed as Dec
type=expr
~~~
# SOURCE
~~~roc
match 3.14 {
    3.14 => "pi"
    2.71 => "e"
    x => "other"
}
~~~
# PROBLEMS
**UNUSED VARIABLE**
Variable ``x`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**pattern_frac_types.md:4:5:4:6:**
```roc
    x => "other"
```
    ^


# TOKENS
~~~zig
KwMatch(1:1-1:6),Float(1:7-1:11),OpenCurly(1:12-1:13),Newline(1:1-1:1),
Float(2:5-2:9),OpFatArrow(2:10-2:12),StringStart(2:13-2:14),StringPart(2:14-2:16),StringEnd(2:16-2:17),Newline(1:1-1:1),
Float(3:5-3:9),OpFatArrow(3:10-3:12),StringStart(3:13-3:14),StringPart(3:14-3:15),StringEnd(3:15-3:16),Newline(1:1-1:1),
LowerIdent(4:5-4:6),OpFatArrow(4:7-4:9),StringStart(4:10-4:11),StringPart(4:11-4:16),StringEnd(4:16-4:17),Newline(1:1-1:1),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-frac @1.7-1.11 (raw "3.14"))
	(branches
		(branch @1.1-1.1
			(p-frac @2.5-2.9 (raw "3.14"))
			(e-string @2.13-2.17
				(e-string-part @2.14-2.16 (raw "pi"))))
		(branch @1.1-1.1
			(p-frac @3.5-3.9 (raw "2.71"))
			(e-string @3.13-3.16
				(e-string-part @3.14-3.15 (raw "e"))))
		(branch @1.1-1.1
			(p-ident @4.5-4.6 (raw "x"))
			(e-string @4.10-4.17
				(e-string-part @4.11-4.16 (raw "other"))))))
~~~
# FORMATTED
~~~roc
match 3.14 {
	3.14 => "pi"
	2.71 => "e"
	x => "other"
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-5.2
	(match @1.1-5.2
		(cond
			(e-dec-small @1.7-1.11 (numerator "314") (denominator-power-of-ten "2") (value "3.14")))
		(branches
			(branch
				(patterns
					(p-small-dec @2.5-2.9 (degenerate false)))
				(value
					(e-string @2.13-2.17
						(e-literal @2.14-2.16 (string "pi")))))
			(branch
				(patterns
					(p-small-dec @3.5-3.9 (degenerate false)))
				(value
					(e-string @3.13-3.16
						(e-literal @3.14-3.15 (string "e")))))
			(branch
				(patterns
					(p-assign @4.5-4.6 (ident "x") (degenerate false)))
				(value
					(e-string @4.10-4.17
						(e-literal @4.11-4.16 (string "other"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "Str"))
~~~
