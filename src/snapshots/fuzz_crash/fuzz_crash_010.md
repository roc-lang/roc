# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
H{o,
    ]
foo =

    "on        (string 'onmo %')))
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
MISMATCHED BRACE - :0:0:0:0
UNCLOSED STRING - :0:0:0:0
MISSING HEADER - fuzz_crash_010.md:1:1:1:2
PARSE ERROR - fuzz_crash_010.md:5:35:5:35
COMPILER DIAGNOSTIC - fuzz_crash_010.md:0:0:0:0
# PROBLEMS
**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.

**MISMATCHED BRACE**
This brace does not match the corresponding opening brace.

**UNCLOSED STRING**
This string is missing a closing quote.

**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_010.md:1:1:1:2:**
```roc
H{o,
```
^


**PARSE ERROR**
A parsing error occurred: `string_unclosed`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_010.md:5:35:5:35:**
```roc
    "on        (string 'onmo %')))
```
                                  


**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'invalid_top_level_statement' is not yet handled in report generation.
**fuzz_crash_010.md:0:0:0:0**

# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:3-1:4),Comma(1:4-1:5),
CloseCurly(2:6-2:7),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),
StringStart(5:5-5:6),StringPart(5:6-5:35),EndOfFile(5:35-5:35),
~~~
# PARSE
~~~clojure
(file @1.1-5.35
	(malformed-header @1.1-1.2 (tag "missing_header"))
	(statements
		(e-record @1.2-2.7
			(field (field "o")))
		(s-decl @3.1-5.35
			(p-ident @3.1-3.4 (raw "foo"))
			(e-string @5.5-5.35
				(e-string-part @5.6-5.35 (raw "on        (string 'onmo %')))"))))))
~~~
# FORMATTED
~~~roc
{
	o,
}
foo = 

	"on        (string 'onmo %')))"
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(def
		(pattern
			(p-assign @3.1-3.4 (ident "foo")))
		(expr
			(e-string @5.5-5.35
				(e-literal @5.6-5.35 (string "on        (string 'onmo %')))"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.4 (type "Str")))
	(expressions
		(expr @5.5-5.35 (type "Str"))))
~~~
