# META
~~~ini
description=Multiline formatting package
type=file
~~~
# SOURCE
~~~roc
package
	[
		a!,
		b!,
	]
	{
		a: "a",
		b: "b",
	}

a! : Str => Str
b! : Str => Str
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - package.md:3:3:3:5
EXPOSED BUT NOT DEFINED - package.md:4:3:4:5
# PROBLEMS
**EXPOSED BUT NOT DEFINED**

**Exposed but Not Defined**
'a!' is exposed in the module header but is not defined:
**package.md:3:3:3:5:**
```roc
		a!,
```
  ^^


**EXPOSED BUT NOT DEFINED**

**Exposed but Not Defined**
'b!' is exposed in the module header but is not defined:
**package.md:4:3:4:5:**
```roc
		b!,
```
  ^^


# TOKENS
~~~zig
KwPackage(1:1-1:8),
OpenSquare(2:2-2:3),
LowerIdent(3:3-3:5),Comma(3:5-3:6),
LowerIdent(4:3-4:5),Comma(4:5-4:6),
CloseSquare(5:2-5:3),
OpenCurly(6:2-6:3),
LowerIdent(7:3-7:4),OpColon(7:4-7:5),StringStart(7:6-7:7),StringPart(7:7-7:8),StringEnd(7:8-7:9),Comma(7:9-7:10),
LowerIdent(8:3-8:4),OpColon(8:4-8:5),StringStart(8:6-8:7),StringPart(8:7-8:8),StringEnd(8:8-8:9),Comma(8:9-8:10),
CloseCurly(9:2-9:3),
LowerIdent(11:1-11:3),OpColon(11:4-11:5),UpperIdent(11:6-11:9),OpFatArrow(11:10-11:12),UpperIdent(11:13-11:16),
LowerIdent(12:1-12:3),OpColon(12:4-12:5),UpperIdent(12:6-12:9),OpFatArrow(12:10-12:12),UpperIdent(12:13-12:16),EndOfFile(12:16-12:16),
~~~
# PARSE
~~~clojure
(file @1.1-12.16
	(package @1.1-9.3
		(exposes @2.2-5.3
			(exposed-lower-ident @3.3-3.5
				(text "a!"))
			(exposed-lower-ident @4.3-4.5
				(text "b!")))
		(packages @6.2-9.3
			(record-field @7.3-7.9 (name "a")
				(e-string @7.6-7.9
					(e-string-part @7.7-7.8 (raw "a"))))
			(record-field @8.3-8.9 (name "b")
				(e-string @8.6-8.9
					(e-string-part @8.7-8.8 (raw "b"))))))
	(statements
		(s-type-anno @11.1-11.16 (name "a!")
			(ty-fn @11.6-11.16
				(ty @11.6-11.9 (name "Str"))
				(ty @11.13-11.16 (name "Str"))))
		(s-type-anno @12.1-12.16 (name "b!")
			(ty-fn @12.6-12.16
				(ty @12.6-12.9 (name "Str"))
				(ty @12.13-12.16 (name "Str"))))))
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
