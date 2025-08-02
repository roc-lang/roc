# META
~~~ini
description=Singleline formatting hosted
type=file
~~~
# SOURCE
~~~roc
hosted [a!, b!]

a! : Str => Str
b! : Str => Str
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - hosted.md:1:9:1:11
EXPOSED BUT NOT DEFINED - hosted.md:1:13:1:15
# PROBLEMS
**EXPOSED BUT NOT DEFINED**
The module header says that `A!` is exposed, but it is not defined anywhere in this module.

**hosted.md:1:9:1:11:**
```roc
hosted [a!, b!]
```
        ^^
You can fix this by either defining `A!` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that `B!` is exposed, but it is not defined anywhere in this module.

**hosted.md:1:13:1:15:**
```roc
hosted [a!, b!]
```
            ^^
You can fix this by either defining `B!` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwHosted(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:11),Comma(1:11-1:12),LowerIdent(1:13-1:15),CloseSquare(1:15-1:16),
LowerIdent(3:1-3:3),OpColon(3:4-3:5),UpperIdent(3:6-3:9),OpFatArrow(3:10-3:12),UpperIdent(3:13-3:16),
LowerIdent(4:1-4:3),OpColon(4:4-4:5),UpperIdent(4:6-4:9),OpFatArrow(4:10-4:12),UpperIdent(4:13-4:16),EndOfFile(4:16-4:16),
~~~
# PARSE
~~~clojure
(file @1.1-4.16
	(hosted @1.1-1.16
		(exposes @1.8-1.16
			(exposed-lower-ident @1.9-1.11
				(text "a!"))
			(exposed-lower-ident @1.13-1.15
				(text "b!"))))
	(statements
		(s-type-anno @3.1-3.16 (name "a!")
			(ty-fn @3.6-3.16
				(ty @3.6-3.9 (name "Str"))
				(ty @3.13-3.16 (name "Str"))))
		(s-type-anno @4.1-4.16 (name "b!")
			(ty-fn @4.6-4.16
				(ty @4.6-4.9 (name "Str"))
				(ty @4.13-4.16 (name "Str"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
