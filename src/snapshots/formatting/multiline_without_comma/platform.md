# META
~~~ini
description=Multiline without comma formatting platform
type=file
~~~
# SOURCE
~~~roc
platform "pf"
	requires {
		R1,
		R2
	} {
		r1 : R1 -> R2,
		r2 : R1 -> R2
	}
	exposes [
		E1,
		E2
	]
	packages {
		pa1: "pa1",
		pa2: "pa2"
	}
	# imports [I1.{ I11, I12, }, I2.{ I21, I22, },]
	provides [
		pr1,
		pr2
	]
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - platform.md:10:3:10:5
EXPOSED BUT NOT DEFINED - platform.md:11:3:11:5
# PROBLEMS
**EXPOSED BUT NOT DEFINED**

**Exposed but Not Defined**
'E1' is exposed in the module header but is not defined:
**platform.md:10:3:10:5:**
```roc
		E1,
```
  ^^


**EXPOSED BUT NOT DEFINED**

**Exposed but Not Defined**
'E2' is exposed in the module header but is not defined:
**platform.md:11:3:11:5:**
```roc
		E2
```
  ^^


# TOKENS
~~~zig
KwPlatform(1:1-1:9),StringStart(1:10-1:11),StringPart(1:11-1:13),StringEnd(1:13-1:14),
KwRequires(2:2-2:10),OpenCurly(2:11-2:12),
UpperIdent(3:3-3:5),Comma(3:5-3:6),
UpperIdent(4:3-4:5),
CloseCurly(5:2-5:3),OpenCurly(5:4-5:5),
LowerIdent(6:3-6:5),OpColon(6:6-6:7),UpperIdent(6:8-6:10),OpArrow(6:11-6:13),UpperIdent(6:14-6:16),Comma(6:16-6:17),
LowerIdent(7:3-7:5),OpColon(7:6-7:7),UpperIdent(7:8-7:10),OpArrow(7:11-7:13),UpperIdent(7:14-7:16),
CloseCurly(8:2-8:3),
KwExposes(9:2-9:9),OpenSquare(9:10-9:11),
UpperIdent(10:3-10:5),Comma(10:5-10:6),
UpperIdent(11:3-11:5),
CloseSquare(12:2-12:3),
KwPackages(13:2-13:10),OpenCurly(13:11-13:12),
LowerIdent(14:3-14:6),OpColon(14:6-14:7),StringStart(14:8-14:9),StringPart(14:9-14:12),StringEnd(14:12-14:13),Comma(14:13-14:14),
LowerIdent(15:3-15:6),OpColon(15:6-15:7),StringStart(15:8-15:9),StringPart(15:9-15:12),StringEnd(15:12-15:13),
CloseCurly(16:2-16:3),
KwProvides(18:2-18:10),OpenSquare(18:11-18:12),
LowerIdent(19:3-19:6),Comma(19:6-19:7),
LowerIdent(20:3-20:6),
CloseSquare(21:2-21:3),EndOfFile(21:3-21:3),
~~~
# PARSE
~~~clojure
(file @1.1-21.3
	(platform @1.1-21.3 (name "pf")
		(rigids @2.11-5.3
			(exposed-upper-ident @3.3-3.5 (text "R1"))
			(exposed-upper-ident @4.3-4.5 (text "R2")))
		(ty-record @5.4-8.3
			(anno-record-field @6.3-6.16 (name "r1")
				(ty-fn @6.8-6.16
					(ty @6.8-6.10 (name "R1"))
					(ty @6.14-6.16 (name "R2"))))
			(anno-record-field @7.3-7.16 (name "r2")
				(ty-fn @7.8-7.16
					(ty @7.8-7.10 (name "R1"))
					(ty @7.14-7.16 (name "R2")))))
		(exposes @9.10-12.3
			(exposed-upper-ident @10.3-10.5 (text "E1"))
			(exposed-upper-ident @11.3-11.5 (text "E2")))
		(packages @13.11-16.3
			(record-field @14.3-14.13 (name "pa1")
				(e-string @14.8-14.13
					(e-string-part @14.9-14.12 (raw "pa1"))))
			(record-field @15.3-15.13 (name "pa2")
				(e-string @15.8-15.13
					(e-string-part @15.9-15.12 (raw "pa2")))))
		(provides @18.11-21.3
			(exposed-lower-ident @19.3-19.6
				(text "pr1"))
			(exposed-lower-ident @20.3-20.6
				(text "pr2"))))
	(statements))
~~~
# FORMATTED
~~~roc
platform "pf"
	requires {
		R1,
		R2,
	} {
		r1 : R1 -> R2,
		r2 : R1 -> R2,
	}
	exposes [
		E1,
		E2,
	]
	packages {
		pa1: "pa1",
		pa2: "pa2",
	}
	# imports [I1.{ I11, I12, }, I2.{ I21, I22, },]
	provides [
		pr1,
		pr2,
	]
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
