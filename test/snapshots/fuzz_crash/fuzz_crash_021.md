# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
Fli/main.roc" }

Pair(a, b+ : (
~~~
# EXPECTED
UNCLOSED STRING - fuzz_crash_021.md:1:13:1:16
PARSE ERROR - fuzz_crash_021.md:1:4:1:5
PARSE ERROR - fuzz_crash_021.md:1:5:1:9
PARSE ERROR - fuzz_crash_021.md:1:9:1:13
PARSE ERROR - fuzz_crash_021.md:1:13:1:14
PARSE ERROR - fuzz_crash_021.md:1:14:1:16
PARSE ERROR - fuzz_crash_021.md:1:16:1:16
PARSE ERROR - fuzz_crash_021.md:3:1:3:5
PARSE ERROR - fuzz_crash_021.md:4:1:4:1
MALFORMED TYPE - fuzz_crash_021.md:3:1:3:11
TYPE MODULE MISSING MATCHING TYPE - fuzz_crash_021.md:1:1:3:15
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

**fuzz_crash_021.md:1:13:1:16:**
```roc
Fli/main.roc" }
```
            ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:1:1:1:4:**
```roc
Fli/main.roc" }
```
^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:1:4:1:5:**
```roc
Fli/main.roc" }
```
   ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:1:5:1:9:**
```roc
Fli/main.roc" }
```
    ^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:1:9:1:13:**
```roc
Fli/main.roc" }
```
        ^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:1:13:1:14:**
```roc
Fli/main.roc" }
```
            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:1:14:1:16:**
```roc
Fli/main.roc" }
```
             ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:1:16:1:16:**
```roc
Fli/main.roc" }
```
               ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:3:1:3:5:**
```roc
Pair(a, b+ : (
```
^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:3:5:3:6:**
```roc
Pair(a, b+ : (
```
    ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:3:6:3:7:**
```roc
Pair(a, b+ : (
```
     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:3:7:3:8:**
```roc
Pair(a, b+ : (
```
      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:3:9:3:10:**
```roc
Pair(a, b+ : (
```
        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:3:10:3:11:**
```roc
Pair(a, b+ : (
```
         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:3:12:3:13:**
```roc
Pair(a, b+ : (
```
           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:3:14:3:15:**
```roc
Pair(a, b+ : (
```
             ^


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**fuzz_crash_021.md:1:1:3:15:**
```roc
Fli/main.roc" }

Pair(a, b+ : (
```


# TOKENS
~~~zig
UpperIdent,OpSlash,LowerIdent,NoSpaceDotLowerIdent,StringStart,StringPart,StringEnd,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,OpPlus,OpColon,OpenRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc


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
