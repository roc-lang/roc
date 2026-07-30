# META
~~~ini
description=Json.parse decodes string escape sequences; Json.to_str escapes them
type=repl
~~~
# SOURCE
~~~roc
» match Json.parse("\"say \\\"hi\\\" on line one\\nline two\"") { Ok(s) => s, Err(_) => "PARSE ERROR" }
» match Json.parse("\"caf\\u00e9 \\uD83D\\uDE00\"") { Ok(s) => s, Err(_) => "PARSE ERROR" }
» match Json.parse("\"lone high surrogate \\uD800\"") { Ok(s) => s, Err(_) => "PARSE ERROR" }
» match Json.parse("\"unknown \\x escape\"") { Ok(s) => s, Err(_) => "PARSE ERROR" }
» Json.to_str("quote \" backslash \\ tab \t")
» match Json.parse("\"tab\\there \\/ slash\"") { Ok(s) => s, Err(_) => "PARSE ERROR" }
» match Json.parse("\"\\b\\f\\r\"") { Ok(s) => Str.count_utf8_bytes(s), Err(_) => 999 }
» match Json.parse("\"\\u12\"") { Ok(s) => s, Err(_) => "PARSE ERROR" }
» match Json.parse("\"\\uZZZZ\"") { Ok(s) => s, Err(_) => "PARSE ERROR" }
» match Json.parse("\"ends with \\") { Ok(s) => s, Err(_) => "PARSE ERROR" }
» match Json.parse("\"no end \\\"") { Ok(s) => s, Err(_) => "PARSE ERROR" }
» Json.to_str("ctrl \u(1) café 中")
» Json.parse(Json.to_str("say \"hi\"\ttab \\ slash")) == Ok("say \"hi\"\ttab \\ slash")
~~~
# OUTPUT
"say \"hi\" on line one
line two"
---
"café 😀"
---
"PARSE ERROR"
---
"PARSE ERROR"
---
"\"quote \\\" backslash \\\\ tab \\t\""
---
"tab	here / slash"
---
3
---
"PARSE ERROR"
---
"PARSE ERROR"
---
"PARSE ERROR"
---
"PARSE ERROR"
---
"\"ctrl \\u0001 café 中\""
---
True
# PROBLEMS
NIL
