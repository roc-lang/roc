# META
~~~ini
description=Platform header with malformed for clause
type=file
~~~
# SOURCE
~~~roc
platform "test-platform" requires { [ : model] for main : {} -> {} }
~~~
# EXPECTED
PARSE ERROR - parse_error_platform_for_clause.md:1:39:1:40
PARSE ERROR - parse_error_platform_for_clause.md:1:41:1:46
PARSE ERROR - parse_error_platform_for_clause.md:1:46:1:47
PARSE ERROR - parse_error_platform_for_clause.md:1:57:1:58
PARSE ERROR - parse_error_platform_for_clause.md:1:59:1:60
PARSE ERROR - parse_error_platform_for_clause.md:1:60:1:61
PARSE ERROR - parse_error_platform_for_clause.md:1:62:1:64
PARSE ERROR - parse_error_platform_for_clause.md:1:65:1:66
PARSE ERROR - parse_error_platform_for_clause.md:1:66:1:67
PARSE ERROR - parse_error_platform_for_clause.md:1:68:1:69
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_for_clause_alias_name`
This is an unexpected parsing error. Please check your syntax.

**parse_error_platform_for_clause.md:1:39:1:40:**
```roc
platform "test-platform" requires { [ : model] for main : {} -> {} }
```
                                      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_error_platform_for_clause.md:1:41:1:46:**
```roc
platform "test-platform" requires { [ : model] for main : {} -> {} }
```
                                        ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_error_platform_for_clause.md:1:46:1:47:**
```roc
platform "test-platform" requires { [ : model] for main : {} -> {} }
```
                                             ^


**PARSE ERROR**
A parsing error occurred: `for_expected_in`
This is an unexpected parsing error. Please check your syntax.

**parse_error_platform_for_clause.md:1:57:1:58:**
```roc
platform "test-platform" requires { [ : model] for main : {} -> {} }
```
                                                        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_error_platform_for_clause.md:1:59:1:60:**
```roc
platform "test-platform" requires { [ : model] for main : {} -> {} }
```
                                                          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_error_platform_for_clause.md:1:60:1:61:**
```roc
platform "test-platform" requires { [ : model] for main : {} -> {} }
```
                                                           ^


**PARSE ERROR**
Function types with multiple arrows need parentheses.

Instead of writing **a -> b -> c**, use parentheses to clarify which you mean:
        a -> (b -> c) for a **curried** function (a function that **returns** another function)
        (a -> b) -> c for a **higher-order** function (a function that **takes** another function)

**parse_error_platform_for_clause.md:1:62:1:64:**
```roc
platform "test-platform" requires { [ : model] for main : {} -> {} }
```
                                                             ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_error_platform_for_clause.md:1:65:1:66:**
```roc
platform "test-platform" requires { [ : model] for main : {} -> {} }
```
                                                                ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_error_platform_for_clause.md:1:66:1:67:**
```roc
platform "test-platform" requires { [ : model] for main : {} -> {} }
```
                                                                 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_error_platform_for_clause.md:1:68:1:69:**
```roc
platform "test-platform" requires { [ : model] for main : {} -> {} }
```
                                                                   ^


# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,KwRequires,OpenCurly,OpenSquare,OpColon,LowerIdent,CloseSquare,KwFor,LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,OpenCurly,CloseCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "expected_for_clause_alias_name"))
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "for_expected_in"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "multi_arrow_needs_parens"))
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
