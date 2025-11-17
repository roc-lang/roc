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
PARSE ERROR - fuzz_crash_002.md:1:7:1:9
PARSE ERROR - fuzz_crash_002.md:1:9:1:11
PARSE ERROR - fuzz_crash_002.md:1:11:1:13
PARSE ERROR - fuzz_crash_002.md:1:13:1:15
PARSE ERROR - fuzz_crash_002.md:1:15:1:17
PARSE ERROR - fuzz_crash_002.md:1:17:1:19
PARSE ERROR - fuzz_crash_002.md:1:19:1:21
PARSE ERROR - fuzz_crash_002.md:1:21:1:23
PARSE ERROR - fuzz_crash_002.md:1:23:1:24
PARSE ERROR - fuzz_crash_002.md:1:24:1:25
MALFORMED TYPE - fuzz_crash_002.md:1:6:1:7
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

**fuzz_crash_002.md:1:7:1:9:**
```roc
modu:;::::::::::::::le[%
```
      ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:9:1:11:**
```roc
modu:;::::::::::::::le[%
```
        ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:11:1:13:**
```roc
modu:;::::::::::::::le[%
```
          ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:13:1:15:**
```roc
modu:;::::::::::::::le[%
```
            ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:15:1:17:**
```roc
modu:;::::::::::::::le[%
```
              ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:17:1:19:**
```roc
modu:;::::::::::::::le[%
```
                ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:19:1:21:**
```roc
modu:;::::::::::::::le[%
```
                  ^^


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


# TOKENS
~~~zig
LowerIdent,OpColon,MalformedUnknownToken,OpDoubleColon,OpDoubleColon,OpDoubleColon,OpDoubleColon,OpDoubleColon,OpDoubleColon,OpDoubleColon,LowerIdent,OpenSquare,OpPercent,
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
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
modu : 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "modu"))
		(e-anno-only)
		(annotation
			(ty-malformed))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
