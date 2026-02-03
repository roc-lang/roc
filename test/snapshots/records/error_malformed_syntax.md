# META
~~~ini
description=Malformed record syntax (error case)
type=snippet
~~~
# SOURCE
~~~roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
~~~
# EXPECTED
PARSE ERROR - error_malformed_syntax.md:1:1:1:2
UNEXPECTED TOKEN IN TYPE ANNOTATION - error_malformed_syntax.md:1:9:1:10
PARSE ERROR - error_malformed_syntax.md:1:10:1:15
PARSE ERROR - error_malformed_syntax.md:1:15:1:16
PARSE ERROR - error_malformed_syntax.md:1:16:1:17
PARSE ERROR - error_malformed_syntax.md:1:18:1:19
PARSE ERROR - error_malformed_syntax.md:1:20:1:22
PARSE ERROR - error_malformed_syntax.md:1:22:1:23
PARSE ERROR - error_malformed_syntax.md:1:24:1:25
UNEXPECTED TOKEN IN TYPE ANNOTATION - error_malformed_syntax.md:1:33:1:34
PARSE ERROR - error_malformed_syntax.md:1:35:1:41
PARSE ERROR - error_malformed_syntax.md:1:46:1:51
PARSE ERROR - error_malformed_syntax.md:1:51:1:52
PARSE ERROR - error_malformed_syntax.md:1:53:1:54
PARSE ERROR - error_malformed_syntax.md:1:54:1:61
PARSE ERROR - error_malformed_syntax.md:1:61:1:62
PARSE ERROR - error_malformed_syntax.md:1:62:1:63
PARSE ERROR - error_malformed_syntax.md:1:64:1:69
PARSE ERROR - error_malformed_syntax.md:1:69:1:70
PARSE ERROR - error_malformed_syntax.md:1:71:1:73
PARSE ERROR - error_malformed_syntax.md:1:73:1:74
PARSE ERROR - error_malformed_syntax.md:1:75:1:76
PARSE ERROR - error_malformed_syntax.md:1:76:1:86
PARSE ERROR - error_malformed_syntax.md:1:86:1:87
PARSE ERROR - error_malformed_syntax.md:1:87:1:88
PARSE ERROR - error_malformed_syntax.md:1:89:1:90
PARSE ERROR - error_malformed_syntax.md:1:91:1:92
MALFORMED TYPE - error_malformed_syntax.md:1:9:1:10
MALFORMED TYPE - error_malformed_syntax.md:1:33:1:34
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:1:1:2:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **"** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**error_malformed_syntax.md:1:9:1:10:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:10:1:15:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
         ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:15:1:16:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
              ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:16:1:17:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
               ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:18:1:19:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:20:1:22:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                   ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:22:1:23:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:24:1:25:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                       ^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **,** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**error_malformed_syntax.md:1:33:1:34:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                                ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:35:1:41:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                                  ^^^^^^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**error_malformed_syntax.md:1:46:1:51:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                                             ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:51:1:52:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                                                  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:53:1:54:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                                                    ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:54:1:61:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                                                     ^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:61:1:62:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                                                            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:62:1:63:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                                                             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:64:1:69:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                                                               ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:69:1:70:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                                                                    ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:71:1:73:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                                                                      ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:73:1:74:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                                                                        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:75:1:76:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                                                                          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:76:1:86:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                                                                           ^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:86:1:87:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                                                                                     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:87:1:88:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                                                                                      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:89:1:90:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                                                                                        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:91:1:92:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                                                                                          ^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**error_malformed_syntax.md:1:9:1:10:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
        ^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**error_malformed_syntax.md:1:33:1:34:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                                ^


# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,OpColon,Int,Comma,Comma,LowerIdent,OpColon,Comma,LowerIdent,UpperIdent,NoSpaceDotLowerIdent,Comma,StringStart,StringPart,StringEnd,OpColon,LowerIdent,Comma,Int,OpColon,StringStart,StringPart,StringEnd,Comma,OpColon,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "name")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "email")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
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
name : 
email : 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "name"))
		(e-anno-only)
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "email"))
		(e-anno-only)
		(annotation
			(ty-malformed))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
