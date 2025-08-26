# META
~~~ini
description=Singleline with comma formatting platform
type=file
~~~
# SOURCE
~~~roc
platform "pf"
	requires { R1, R2, } { r1 : R1 -> R2, r2 : R1 -> R2, }
	exposes [E1, E2,]
	packages { pa1: "pa1", pa2: "pa2", }
	# imports [I1.{ I11, I12, }, I2.{ I21, I22, },]
	provides [pr1, pr2,]
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - platform.md:3:11:3:13
EXPOSED BUT NOT DEFINED - platform.md:3:15:3:17
# PROBLEMS
**EXPOSED BUT NOT DEFINED**
The module header says that `E1` is exposed, but it is not defined anywhere in this module.

**platform.md:3:11:3:13:**
```roc
	exposes [E1, E2,]
```
	         ^^
You can fix this by either defining `E1` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that `E2` is exposed, but it is not defined anywhere in this module.

**platform.md:3:15:3:17:**
```roc
	exposes [E1, E2,]
```
	             ^^
You can fix this by either defining `E2` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwPlatform(1:1-1:9),StringStart(1:10-1:11),StringPart(1:11-1:13),StringEnd(1:13-1:14),
KwRequires(2:2-2:10),OpenCurly(2:11-2:12),UpperIdent(2:13-2:15),Comma(2:15-2:16),UpperIdent(2:17-2:19),Comma(2:19-2:20),CloseCurly(2:21-2:22),OpenCurly(2:23-2:24),LowerIdent(2:25-2:27),OpColon(2:28-2:29),UpperIdent(2:30-2:32),OpArrow(2:33-2:35),UpperIdent(2:36-2:38),Comma(2:38-2:39),LowerIdent(2:40-2:42),OpColon(2:43-2:44),UpperIdent(2:45-2:47),OpArrow(2:48-2:50),UpperIdent(2:51-2:53),Comma(2:53-2:54),CloseCurly(2:55-2:56),
KwExposes(3:2-3:9),OpenSquare(3:10-3:11),UpperIdent(3:11-3:13),Comma(3:13-3:14),UpperIdent(3:15-3:17),Comma(3:17-3:18),CloseSquare(3:18-3:19),
KwPackages(4:2-4:10),OpenCurly(4:11-4:12),LowerIdent(4:13-4:16),OpColon(4:16-4:17),StringStart(4:18-4:19),StringPart(4:19-4:22),StringEnd(4:22-4:23),Comma(4:23-4:24),LowerIdent(4:25-4:28),OpColon(4:28-4:29),StringStart(4:30-4:31),StringPart(4:31-4:34),StringEnd(4:34-4:35),Comma(4:35-4:36),CloseCurly(4:37-4:38),
KwProvides(6:2-6:10),OpenSquare(6:11-6:12),LowerIdent(6:12-6:15),Comma(6:15-6:16),LowerIdent(6:17-6:20),Comma(6:20-6:21),CloseSquare(6:21-6:22),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.22
	(platform @1.1-6.22 (name "pf")
		(rigids @2.11-2.22
			(exposed-upper-ident @2.13-2.15 (text "R1"))
			(exposed-upper-ident @2.17-2.19 (text "R2")))
		(ty-record @2.23-2.56
			(anno-record-field @2.25-2.38 (name "r1")
				(ty-fn @2.30-2.38
					(ty @2.30-2.32 (name "R1"))
					(ty @2.36-2.38 (name "R2"))))
			(anno-record-field @2.40-2.53 (name "r2")
				(ty-fn @2.45-2.53
					(ty @2.45-2.47 (name "R1"))
					(ty @2.51-2.53 (name "R2")))))
		(exposes @3.10-3.19
			(exposed-upper-ident @3.11-3.13 (text "E1"))
			(exposed-upper-ident @3.15-3.17 (text "E2")))
		(packages @4.11-4.38
			(record-field @4.13-4.23 (name "pa1")
				(e-string @4.18-4.23
					(e-string-part @4.19-4.22 (raw "pa1"))))
			(record-field @4.25-4.35 (name "pa2")
				(e-string @4.30-4.35
					(e-string-part @4.31-4.34 (raw "pa2")))))
		(provides @6.11-6.22
			(exposed-lower-ident @6.12-6.15
				(text "pr1"))
			(exposed-lower-ident @6.17-6.20
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
