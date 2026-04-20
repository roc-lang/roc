# META
~~~ini
description=e_anno_only should crash when value is used
type=repl
~~~
# SOURCE
~~~roc
» foo : Str -> Str
» foo("test")
~~~
# OUTPUT
Parse error: Type annotations are not supported in the REPL yet
---
**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

**repl:1:1:1:4:**
```roc
foo("test")
```
^^^
# PROBLEMS
NIL
