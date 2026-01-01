# META
~~~ini
description=App header with multiple platforms
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "a", pf2: platform "b" }
~~~
# EXPECTED
MULTIPLE PLATFORMS - parse_error_app_multi_platform.md:1:1:1:4
PARSE ERROR - parse_error_app_multi_platform.md:1:46:1:47
PARSE ERROR - parse_error_app_multi_platform.md:1:47:1:48
PARSE ERROR - parse_error_app_multi_platform.md:1:48:1:49
PARSE ERROR - parse_error_app_multi_platform.md:1:50:1:51
# PROBLEMS
**MULTIPLE PLATFORMS**
Only one platform declaration is allowed per file.
Remove the duplicate platform declaration.

**parse_error_app_multi_platform.md:1:1:1:4:**
```roc
app [main] { pf: platform "a", pf2: platform "b" }
```
^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_error_app_multi_platform.md:1:46:1:47:**
```roc
app [main] { pf: platform "a", pf2: platform "b" }
```
                                             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_error_app_multi_platform.md:1:47:1:48:**
```roc
app [main] { pf: platform "a", pf2: platform "b" }
```
                                              ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_error_app_multi_platform.md:1:48:1:49:**
```roc
app [main] { pf: platform "a", pf2: platform "b" }
```
                                               ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_error_app_multi_platform.md:1:50:1:51:**
```roc
app [main] { pf: platform "a", pf2: platform "b" }
```
                                                 ^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "multiple_platforms"))
	(statements
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
