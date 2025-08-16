# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]C:k||match 0{0|#
0"
}
~~~
# EXPECTED
UNCLOSED STRING - :0:0:0:0
PARSE ERROR - fuzz_crash_060.md:1:12:1:13
PARSE ERROR - fuzz_crash_060.md:1:13:1:14
PARSE ERROR - fuzz_crash_060.md:1:14:1:19
PARSE ERROR - fuzz_crash_060.md:1:20:1:21
PARSE ERROR - fuzz_crash_060.md:1:21:1:22
PARSE ERROR - fuzz_crash_060.md:1:22:1:23
PARSE ERROR - fuzz_crash_060.md:1:23:1:24
PARSE ERROR - fuzz_crash_060.md:2:1:2:2
PARSE ERROR - fuzz_crash_060.md:2:2:2:3
PARSE ERROR - fuzz_crash_060.md:2:3:2:3
PARSE ERROR - fuzz_crash_060.md:2:3:2:3
PARSE ERROR - fuzz_crash_060.md:3:1:3:2
UNDECLARED TYPE VARIABLE - fuzz_crash_060.md:1:11:1:12
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.```roc
0"
```
 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_060.md:1:12:1:13:**
```roc
module[]C:k||match 0{0|#
```
           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_060.md:1:13:1:14:**
```roc
module[]C:k||match 0{0|#
```
            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_060.md:1:14:1:19:**
```roc
module[]C:k||match 0{0|#
```
             ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_060.md:1:20:1:21:**
```roc
module[]C:k||match 0{0|#
```
                   ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_060.md:1:21:1:22:**
```roc
module[]C:k||match 0{0|#
```
                    ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_060.md:1:22:1:23:**
```roc
module[]C:k||match 0{0|#
```
                     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_060.md:1:23:1:24:**
```roc
module[]C:k||match 0{0|#
```
                      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_060.md:2:1:2:2:**
```roc
0"
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_060.md:2:2:2:3:**
```roc
0"
```
 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_060.md:2:3:2:3:**
```roc
0"
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_060.md:2:3:2:3:**
```roc
0"
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_060.md:3:1:3:2:**
```roc
}
```
^


**UNDECLARED TYPE VARIABLE**
The type variable _k_ is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**fuzz_crash_060.md:1:11:1:12:**
```roc
module[]C:k||match 0{0|#
```
          ^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),UpperIdent(1:9-1:10),OpColon(1:10-1:11),LowerIdent(1:11-1:12),OpBar(1:12-1:13),OpBar(1:13-1:14),KwMatch(1:14-1:19),Int(1:20-1:21),OpenCurly(1:21-1:22),Int(1:22-1:23),OpBar(1:23-1:24),
Int(2:1-2:2),StringStart(2:2-2:3),StringPart(2:3-2:3),StringEnd(2:3-2:3),
CloseCurly(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(file @1.1-3.2
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(s-type-decl @1.9-1.12
			(header @1.9-1.10 (name "C")
				(args))
			(ty-var @1.11-1.12 (raw "k")))
		(s-malformed @1.12-1.13 (tag "statement_unexpected_token"))
		(s-malformed @1.13-1.14 (tag "statement_unexpected_token"))
		(s-malformed @1.14-1.19 (tag "statement_unexpected_token"))
		(s-malformed @1.20-1.21 (tag "statement_unexpected_token"))
		(s-malformed @1.21-1.22 (tag "statement_unexpected_token"))
		(s-malformed @1.22-1.23 (tag "statement_unexpected_token"))
		(s-malformed @1.23-1.24 (tag "statement_unexpected_token"))
		(s-malformed @2.1-2.2 (tag "statement_unexpected_token"))
		(s-malformed @2.2-2.3 (tag "statement_unexpected_token"))
		(s-malformed @2.3-2.3 (tag "statement_unexpected_token"))
		(s-malformed @2.3-2.3 (tag "statement_unexpected_token"))
		(s-malformed @3.1-3.2 (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module []
C : k



~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @1.9-1.12
		(ty-header @1.9-1.10 (name "C"))
		(ty-malformed @1.11-1.12)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @1.9-1.12 (type "Error")
			(ty-header @1.9-1.10 (name "C"))))
	(expressions))
~~~
