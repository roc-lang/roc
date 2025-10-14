# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
modu:;::::::::::::::le[%
~~~
# EXPECTED
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_002.md:1:6:1:7
PARSE ERROR - fuzz_crash_002.md:1:7:1:8
PARSE ERROR - fuzz_crash_002.md:1:8:1:9
PARSE ERROR - fuzz_crash_002.md:1:9:1:10
PARSE ERROR - fuzz_crash_002.md:1:10:1:11
PARSE ERROR - fuzz_crash_002.md:1:11:1:12
PARSE ERROR - fuzz_crash_002.md:1:12:1:13
PARSE ERROR - fuzz_crash_002.md:1:13:1:14
PARSE ERROR - fuzz_crash_002.md:1:14:1:15
PARSE ERROR - fuzz_crash_002.md:1:15:1:16
PARSE ERROR - fuzz_crash_002.md:1:16:1:17
PARSE ERROR - fuzz_crash_002.md:1:17:1:18
PARSE ERROR - fuzz_crash_002.md:1:18:1:19
PARSE ERROR - fuzz_crash_002.md:1:19:1:20
PARSE ERROR - fuzz_crash_002.md:1:20:1:21
PARSE ERROR - fuzz_crash_002.md:1:21:1:23
PARSE ERROR - fuzz_crash_002.md:1:23:1:24
PARSE ERROR - fuzz_crash_002.md:1:24:1:25
MALFORMED TYPE - fuzz_crash_002.md:1:6:1:7
MISSING MAIN! FUNCTION - fuzz_crash_002.md:1:1:1:25
# PROBLEMS
**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **;** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**fuzz_crash_002.md:1:6:1:7:**
```roc
modu:;::::::::::::::le[%
```
     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:7:1:8:**
```roc
modu:;::::::::::::::le[%
```
      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:8:1:9:**
```roc
modu:;::::::::::::::le[%
```
       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:9:1:10:**
```roc
modu:;::::::::::::::le[%
```
        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:10:1:11:**
```roc
modu:;::::::::::::::le[%
```
         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:11:1:12:**
```roc
modu:;::::::::::::::le[%
```
          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:12:1:13:**
```roc
modu:;::::::::::::::le[%
```
           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:13:1:14:**
```roc
modu:;::::::::::::::le[%
```
            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:14:1:15:**
```roc
modu:;::::::::::::::le[%
```
             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:15:1:16:**
```roc
modu:;::::::::::::::le[%
```
              ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:16:1:17:**
```roc
modu:;::::::::::::::le[%
```
               ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:17:1:18:**
```roc
modu:;::::::::::::::le[%
```
                ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:18:1:19:**
```roc
modu:;::::::::::::::le[%
```
                 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:19:1:20:**
```roc
modu:;::::::::::::::le[%
```
                  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:20:1:21:**
```roc
modu:;::::::::::::::le[%
```
                   ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:21:1:23:**
```roc
modu:;::::::::::::::le[%
```
                    ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:23:1:24:**
```roc
modu:;::::::::::::::le[%
```
                      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:24:1:25:**
```roc
modu:;::::::::::::::le[%
```
                       ^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**fuzz_crash_002.md:1:6:1:7:**
```roc
modu:;::::::::::::::le[%
```
     ^


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**fuzz_crash_002.md:1:1:1:25:**
```roc
modu:;::::::::::::::le[%
```
^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
LowerIdent,OpColon,MalformedUnknownToken,OpColon,OpColon,OpColon,OpColon,OpColon,OpColon,OpColon,OpColon,OpColon,OpColon,OpColon,OpColon,OpColon,OpColon,LowerIdent,OpenSquare,OpPercent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "modu")
			(ty-malformed (tag "ty_anno_unexpected_token")))
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
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
modu : 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-type-anno (name "modu")
		(ty-malformed)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
