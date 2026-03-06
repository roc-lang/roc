# META
~~~ini
description=String ordering operations should fail gracefully (not supported)
type=repl
~~~
# SOURCE
~~~roc
» "apple" > "banana"
» "zoo" < "aardvark"
» "equal" >= "equal"
» "first" <= "second"
~~~
# OUTPUT
**MISSING METHOD**
The value before this **>** operator has a type that doesn't have a **is_gt** method:
**repl:1:1:1:19:**
```roc
"apple" > "banana"
```
^^^^^^^^^^^^^^^^^^

The value's type, which does not have a method named **is_gt**, is:

    Str

**Hint:** The **>** operator calls a method named **is_gt** on the value preceding it, passing the value after the operator as the one argument.
---
**MISSING METHOD**
The value before this **<** operator has a type that doesn't have a **is_lt** method:
**repl:1:1:1:19:**
```roc
"zoo" < "aardvark"
```
^^^^^^^^^^^^^^^^^^

The value's type, which does not have a method named **is_lt**, is:

    Str

**Hint:** The **<** operator calls a method named **is_lt** on the value preceding it, passing the value after the operator as the one argument.
---
**MISSING METHOD**
The value before this **>=** operator has a type that doesn't have a **is_gte** method:
**repl:1:1:1:19:**
```roc
"equal" >= "equal"
```
^^^^^^^^^^^^^^^^^^

The value's type, which does not have a method named **is_gte**, is:

    Str

**Hint:** The **>=** operator calls a method named **is_gte** on the value preceding it, passing the value after the operator as the one argument.
---
**MISSING METHOD**
The value before this **<=** operator has a type that doesn't have a **is_lte** method:
**repl:1:1:1:20:**
```roc
"first" <= "second"
```
^^^^^^^^^^^^^^^^^^^

The value's type, which does not have a method named **is_lte**, is:

    Str

**Hint:** The **<=** operator calls a method named **is_lte** on the value preceding it, passing the value after the operator as the one argument.
# PROBLEMS
NIL
