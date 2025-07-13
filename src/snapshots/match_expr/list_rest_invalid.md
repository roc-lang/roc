# META
~~~ini
description=Match expression with invalid (old style) list rest patterns should error
type=expr
~~~
# SOURCE
~~~roc
match items {
    [first, ..rest] => 0 # invalid rest pattern should error
    [..rest, last] => 1 # invalid rest pattern should error
    [x, ..rest, y] => 2 # invalid rest pattern should error
}
~~~
# EXPECTED
BAD LIST REST PATTERN SYNTAX - list_rest_invalid.md:2:13:2:19
BAD LIST REST PATTERN SYNTAX - list_rest_invalid.md:3:6:3:12
BAD LIST REST PATTERN SYNTAX - list_rest_invalid.md:4:9:4:15
UNDEFINED VARIABLE - list_rest_invalid.md:1:7:1:12
UNUSED VARIABLE - list_rest_invalid.md:2:6:2:11
UNUSED VARIABLE - list_rest_invalid.md:2:15:2:15
UNUSED VARIABLE - list_rest_invalid.md:3:8:3:8
UNUSED VARIABLE - list_rest_invalid.md:3:14:3:18
UNUSED VARIABLE - list_rest_invalid.md:4:11:4:11
UNUSED VARIABLE - list_rest_invalid.md:4:6:4:7
UNUSED VARIABLE - list_rest_invalid.md:4:17:4:18
# PROBLEMS
**BAD LIST REST PATTERN SYNTAX**
List rest patterns should use the `.. as name` syntax, not `..name`.
For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

Here is the problematic code:
**list_rest_invalid.md:2:13:2:19:**
```roc
    [first, ..rest] => 0 # invalid rest pattern should error
```
            ^^^^^^


**BAD LIST REST PATTERN SYNTAX**
List rest patterns should use the `.. as name` syntax, not `..name`.
For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

Here is the problematic code:
**list_rest_invalid.md:3:6:3:12:**
```roc
    [..rest, last] => 1 # invalid rest pattern should error
```
     ^^^^^^


**BAD LIST REST PATTERN SYNTAX**
List rest patterns should use the `.. as name` syntax, not `..name`.
For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

Here is the problematic code:
**list_rest_invalid.md:4:9:4:15:**
```roc
    [x, ..rest, y] => 2 # invalid rest pattern should error
```
        ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `items` in this scope.
Is there an `import` or `exposing` missing up-top?

**list_rest_invalid.md:1:7:1:12:**
```roc
match items {
```
      ^^^^^


**UNUSED VARIABLE**
Variable ``first`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_first` to suppress this warning.
The unused variable is declared here:
**list_rest_invalid.md:2:6:2:11:**
```roc
    [first, ..rest] => 0 # invalid rest pattern should error
```
     ^^^^^


**UNUSED VARIABLE**
Variable ``rest`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**list_rest_invalid.md:2:15:2:15:**
```roc
    [first, ..rest] => 0 # invalid rest pattern should error
```
              


**UNUSED VARIABLE**
Variable ``rest`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**list_rest_invalid.md:3:8:3:8:**
```roc
    [..rest, last] => 1 # invalid rest pattern should error
```
       


**UNUSED VARIABLE**
Variable ``last`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_last` to suppress this warning.
The unused variable is declared here:
**list_rest_invalid.md:3:14:3:18:**
```roc
    [..rest, last] => 1 # invalid rest pattern should error
```
             ^^^^


**UNUSED VARIABLE**
Variable ``rest`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**list_rest_invalid.md:4:11:4:11:**
```roc
    [x, ..rest, y] => 2 # invalid rest pattern should error
```
          


**UNUSED VARIABLE**
Variable ``x`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**list_rest_invalid.md:4:6:4:7:**
```roc
    [x, ..rest, y] => 2 # invalid rest pattern should error
```
     ^


**UNUSED VARIABLE**
Variable ``y`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_y` to suppress this warning.
The unused variable is declared here:
**list_rest_invalid.md:4:17:4:18:**
```roc
    [x, ..rest, y] => 2 # invalid rest pattern should error
```
                ^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:12),OpenCurly(1:13-1:14),
OpenSquare(2:5-2:6),LowerIdent(2:6-2:11),Comma(2:11-2:12),DoubleDot(2:13-2:15),LowerIdent(2:15-2:19),CloseSquare(2:19-2:20),OpFatArrow(2:21-2:23),Int(2:24-2:25),
OpenSquare(3:5-3:6),DoubleDot(3:6-3:8),LowerIdent(3:8-3:12),Comma(3:12-3:13),LowerIdent(3:14-3:18),CloseSquare(3:18-3:19),OpFatArrow(3:20-3:22),Int(3:23-3:24),
OpenSquare(4:5-4:6),LowerIdent(4:6-4:7),Comma(4:7-4:8),DoubleDot(4:9-4:11),LowerIdent(4:11-4:15),Comma(4:15-4:16),LowerIdent(4:17-4:18),CloseSquare(4:18-4:19),OpFatArrow(4:20-4:22),Int(4:23-4:24),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.12 (raw "items"))
	(branches
		(branch @2.5-2.25
			(p-list @2.5-2.20
				(p-ident @2.6-2.11 (raw "first"))
				(p-list-rest @2.13-2.19 (name "rest")))
			(e-int @2.24-2.25 (raw "0")))
		(branch @3.5-3.24
			(p-list @3.5-3.19
				(p-list-rest @3.6-3.12 (name "rest"))
				(p-ident @3.14-3.18 (raw "last")))
			(e-int @3.23-3.24 (raw "1")))
		(branch @4.5-4.24
			(p-list @4.5-4.19
				(p-ident @4.6-4.7 (raw "x"))
				(p-list-rest @4.9-4.15 (name "rest"))
				(p-ident @4.17-4.18 (raw "y")))
			(e-int @4.23-4.24 (raw "2")))))
~~~
# FORMATTED
~~~roc
match items {
	[first, .. as rest] => 0 # invalid rest pattern should error
	[.. as rest, last] => 1 # invalid rest pattern should error
	[x, .. as rest, y] => 2
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
						(p-list @2.5-2.20
							(patterns
								(p-assign @2.6-2.11 (ident "first")))
							(rest-at (index 1)
								(p-assign @2.15-2.15 (ident "rest"))))))
				(value
					(e-int @2.24-2.25 (value "0"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @3.5-3.19
							(patterns
								(p-assign @3.14-3.18 (ident "last")))
							(rest-at (index 0)
								(p-assign @3.8-3.8 (ident "rest"))))))
				(value
					(e-int @3.23-3.24 (value "1"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @4.5-4.19
							(patterns
								(p-assign @4.6-4.7 (ident "x"))
								(p-assign @4.17-4.18 (ident "y")))
							(rest-at (index 1)
								(p-assign @4.11-4.11 (ident "rest"))))))
				(value
					(e-int @4.23-4.24 (value "2")))))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "Num(a)"))
~~~
