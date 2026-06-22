# META
~~~ini
description=Issue 9375 - REPL must not panic when doing static dispatch on a non-existent variable
type=repl
~~~
# SOURCE
~~~roc
» lst.map(|_| "zzz ").join_with(" ").trim()
~~~
# OUTPUT
**UNDEFINED VARIABLE**
Nothing is named `lst` in this scope.
Is there an `import` or `exposing` missing up-top?

**repl:1:1:1:4:**
```roc
lst.map(|_| "zzz ").join_with(" ").trim()
```
^^^
# PROBLEMS
NIL
