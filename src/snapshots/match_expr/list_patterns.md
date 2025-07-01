# META
~~~ini
description=Match expression with list patterns including invalid rest pattern
type=expr
~~~
# SOURCE
~~~roc
match numbers {
    [] => acc
    [first, ..rest] => 0 # invalid rest pattern should error
}
~~~
# PROBLEMS
**BAD LIST REST PATTERN SYNTAX**
List rest patterns should use the `.. as name` syntax, not `..name`.
For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

Here is the problematic code:
**list_patterns.md:3:13:3:19:**
```roc
    [first, ..rest] => 0 # invalid rest pattern should error
```
            ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `numbers` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `acc` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNUSED VARIABLE**
Variable ``rest`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**list_patterns.md:3:15:3:19:**
```roc
    [first, ..rest] => 0 # invalid rest pattern should error
```
              ^^^^


**UNUSED VARIABLE**
Variable ``first`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_first` to suppress this warning.
The unused variable is declared here:
**list_patterns.md:3:6:3:11:**
```roc
    [first, ..rest] => 0 # invalid rest pattern should error
```
     ^^^^^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:14),OpenCurly(1:15-1:16),Newline(1:1-1:1),
OpenSquare(2:5-2:6),CloseSquare(2:6-2:7),OpFatArrow(2:8-2:10),LowerIdent(2:11-2:14),Newline(1:1-1:1),
OpenSquare(3:5-3:6),LowerIdent(3:6-3:11),Comma(3:11-3:12),DoubleDot(3:13-3:15),LowerIdent(3:15-3:19),CloseSquare(3:19-3:20),OpFatArrow(3:21-3:23),Int(3:24-3:25),Newline(3:27-3:61),
CloseCurly(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.14 (qaul "") (raw "numbers"))
	(branches
		(branch @2.5-3.6
			(p-list @2.5-2.7)
			(e-ident @2.11-2.14 (qaul "") (raw "acc")))
		(branch @3.5-4.2
			(p-list @3.5-3.20
				(p-ident @3.6-3.11 (raw "first"))
				(p-list-rest @3.13-3.20 (name "rest")))
			(e-int @3.24-3.25 (raw "0")))))
~~~
# FORMATTED
~~~roc
match numbers {
	[] => acc
	[first, .. as rest] => 0
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-4.2
	(match @1.1-4.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(p-list @2.5-2.7 (degenerate false)
						(patterns)))
				(value
					(e-runtime-error (tag "ident_not_in_scope"))))
			(branch
				(patterns
					(p-list @3.5-3.20 (degenerate false)
						(patterns
							(p-assign @3.6-3.11 (ident "first")))
						(rest-at (index 1)
							(p-assign @3.15-3.19 (ident "rest")))))
				(value
					(e-int @3.24-3.25 (value "0")))))))
~~~
# TYPES
~~~clojure
(expr @1.1-4.2 (type "*"))
~~~
