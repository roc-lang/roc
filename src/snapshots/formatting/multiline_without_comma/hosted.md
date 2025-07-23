# META
~~~ini
description=Multiline without comma formatting hosted
type=file
~~~
# SOURCE
~~~roc
hosted [
	a!,
	b!
]

a! : Str => Str
b! : Str => Str
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - hosted.md:2:2:2:4
EXPOSED BUT NOT DEFINED - hosted.md:3:2:3:4
# PROBLEMS
**EXPOSED BUT NOT DEFINED**

**Exposed but Not Defined**
'a!' is exposed in the module header but is not defined:
**hosted.md:2:2:2:4:**
```roc
	a!,
```
 ^^


**EXPOSED BUT NOT DEFINED**

**Exposed but Not Defined**
'b!' is exposed in the module header but is not defined:
**hosted.md:3:2:3:4:**
```roc
	b!
```
 ^^


# TOKENS
~~~zig
KwHosted(1:1-1:7),OpenSquare(1:8-1:9),
LowerIdent(2:2-2:4),Comma(2:4-2:5),
LowerIdent(3:2-3:4),
CloseSquare(4:1-4:2),
LowerIdent(6:1-6:3),OpColon(6:4-6:5),UpperIdent(6:6-6:9),OpFatArrow(6:10-6:12),UpperIdent(6:13-6:16),
LowerIdent(7:1-7:3),OpColon(7:4-7:5),UpperIdent(7:6-7:9),OpFatArrow(7:10-7:12),UpperIdent(7:13-7:16),EndOfFile(7:16-7:16),
~~~
# PARSE
~~~clojure
(file @1.1-7.16
	(hosted @1.1-4.2
		(exposes @1.8-4.2
			(exposed-lower-ident @2.2-2.4
				(text "a!"))
			(exposed-lower-ident @3.2-3.4
				(text "b!"))))
	(statements
		(s-type-anno @6.1-6.16 (name "a!")
			(ty-fn @6.6-6.16
				(ty @6.6-6.9 (name "Str"))
				(ty @6.13-6.16 (name "Str"))))
		(s-type-anno @7.1-7.16 (name "b!")
			(ty-fn @7.6-7.16
				(ty @7.6-7.9 (name "Str"))
				(ty @7.13-7.16 (name "Str"))))))
~~~
# FORMATTED
~~~roc
hosted [
	a!,
	b!,
]

a! : Str => Str
b! : Str => Str
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
