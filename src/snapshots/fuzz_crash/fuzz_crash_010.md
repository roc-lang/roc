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
**fuzz_crash_010.md:1:1:1:3:**
```roc
H{o,
```


**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:3-1:4),Comma(1:4-1:5),Newline(1:1-1:1),
CloseCurly(2:6-2:7),Newline(1:1-1:1),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),Newline(1:1-1:1),
Newline(1:1-1:1),
StringStart(5:5-5:6),StringPart(5:6-5:35),EndOfFile(5:35-5:35),
~~~
# PARSE
~~~clojure
(file @1-1-5-35
	(malformed-header @1-1-1-3 (tag "missing_header"))
	(statements
		(e-record @1-2-2-7
			(field (field "o") (optional false)))
		(s-decl @3-1-5-35
			(p-ident @3-1-3-4 (raw "foo"))
			(e-string @5-5-5-35
				(e-string-part @5-6-5-35 (raw "on        (string 'onmo %')))"))))))
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
	(d-let (id 76)
		(p-assign @3-1-3-4 (ident "foo") (id 73))
		(e-string @5-5-5-35 (id 75)
			(e-literal @5-6-5-35 (string "on        (string 'onmo %')))")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "foo") (type "Str")))
	(expressions
		(expr @5-5-5-35 (type "Str"))))
~~~