# META
~~~ini
description=Simple app header with platform
type=file
~~~

# SOURCE
~~~roc
app [main!] { pf: platform "../main.roc", other: "../../other/main.roc" }
~~~

# TOKENS
~~~zig
KwApp(0-3),OpenSquare(4-5),LowerIdent(5-10),CloseSquare(10-11),OpenCurly(12-13),LowerIdent(14-16),OpColon(16-17),KwPlatform(18-26),StringStart(27-28),StringPart(28-39),StringEnd(39-40),Comma(40-41),LowerIdent(42-47),OpColon(47-48),StringStart(49-50),StringPart(50-70),StringEnd(70-71),CloseCurly(72-73),EndOfFile(73-73)
~~~

# PARSE_AST2
~~~clojure
(file
  (app-header
    (provides (lc "main!" @5))
    (platform (record_literal ((lc "pf" @14), (str_literal_big "<big>" @27), (lc "other" @42), (str_literal_big "<big>" @49)) @12))
    (packages (lc "main!" @5))
  )
)

~~~
