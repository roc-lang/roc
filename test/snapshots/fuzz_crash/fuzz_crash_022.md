# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app [main!] { |f: platform "c" }

UserId : U64

ser : UserId -> Str
getUser = |id| if (id > 1!) "big" else "l"

-ain! = |_| getUser(900)
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_022.md:1:1:1:4
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_022.md:1:19:1:27
PARSE ERROR - fuzz_crash_022.md:1:28:1:29
PARSE ERROR - fuzz_crash_022.md:1:29:1:30
PARSE ERROR - fuzz_crash_022.md:1:30:1:31
PARSE ERROR - fuzz_crash_022.md:1:32:1:33
PARSE ERROR - fuzz_crash_022.md:6:27:6:28
PARSE ERROR - fuzz_crash_022.md:8:1:8:2
MALFORMED TYPE - fuzz_crash_022.md:1:19:1:27
INVALID IF CONDITION - :0:0:0:0
UNUSED VARIABLE - fuzz_crash_022.md:6:12:6:14
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_package_or_platform_name`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_022.md:1:1:1:4:**
```roc
app [main!] { |f: platform "c" }
```
^^^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **platform** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**fuzz_crash_022.md:1:19:1:27:**
```roc
app [main!] { |f: platform "c" }
```
                  ^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_022.md:1:28:1:29:**
```roc
app [main!] { |f: platform "c" }
```
                           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_022.md:1:29:1:30:**
```roc
app [main!] { |f: platform "c" }
```
                            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_022.md:1:30:1:31:**
```roc
app [main!] { |f: platform "c" }
```
                             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_022.md:1:32:1:33:**
```roc
app [main!] { |f: platform "c" }
```
                               ^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_022.md:6:27:6:28:**
```roc
getUser = |id| if (id > 1!) "big" else "l"
```
                          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_022.md:8:1:8:2:**
```roc
-ain! = |_| getUser(900)
```
^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**fuzz_crash_022.md:1:19:1:27:**
```roc
app [main!] { |f: platform "c" }
```
                  ^^^^^^^^


**INVALID IF CONDITION**
The condition in this `if` expression could not be processed.

The condition must be a valid expression that evaluates to a `Bool` value (`Bool.true` or `Bool.false`).

**UNUSED VARIABLE**
Variable `id` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_id` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_022.md:6:12:6:14:**
```roc
getUser = |id| if (id > 1!) "big" else "l"
```
           ^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,OpBar,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
UpperIdent,OpColon,UpperIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,OpenRound,LowerIdent,OpGreaterThan,Int,OpBang,CloseRound,StringStart,StringPart,StringEnd,KwElse,StringStart,StringPart,StringEnd,
OpUnaryMinus,LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "expected_package_or_platform_name"))
	(statements
		(s-type-anno (name "f")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "UserId")
				(args))
			(ty (name "U64")))
		(s-type-anno (name "ser")
			(ty-fn
				(ty (name "UserId"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "getUser"))
			(e-lambda
				(args
					(p-ident (raw "id")))
				(e-if-then-else
					(e-malformed (reason "expected_expr_close_round_or_comma"))
					(e-string
						(e-string-part (raw "big")))
					(e-string
						(e-string-part (raw "l"))))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "ain!"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-ident (raw "getUser"))
					(e-int (raw "900")))))))
~~~
# FORMATTED
~~~roc
f : 


UserId : U64

ser : UserId -> Str
getUser = |id| if  "big" else "l"

ain! = |_| getUser(900)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "getUser"))
		(e-lambda
			(args
				(p-assign (ident "id")))
			(e-runtime-error (tag "if_condition_not_canonicalized"))))
	(d-let
		(p-assign (ident "ain!"))
		(e-closure
			(captures
				(capture (ident "getUser")))
			(e-lambda
				(args
					(p-underscore))
				(e-call
					(e-lookup-local
						(p-assign (ident "getUser")))
					(e-num (value "900"))))))
	(s-alias-decl
		(ty-header (name "UserId"))
		(ty-lookup (name "U64") (builtin)))
	(s-type-anno (name "f")
		(ty-malformed)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> Error"))
		(patt (type "_arg -> Error")))
	(type_decls
		(alias (type "UserId")
			(ty-header (name "UserId"))))
	(expressions
		(expr (type "_arg -> Error"))
		(expr (type "_arg -> Error"))))
~~~
