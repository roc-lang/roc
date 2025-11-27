# META
~~~ini
description=Try equality comparisons using == and !=
type=repl
~~~
# SOURCE
~~~roc
» Try.Ok(1) == Try.Ok(1)
» Try.Ok(1) == Try.Ok(2)
~~~
# OUTPUT
<<<<<<< HEAD
Crash: e_closure: failed to resolve capture value
||||||| 719b878da4
Evaluation error: error.BugUnboxedRigidVar
=======
True
>>>>>>> origin/main
---
<<<<<<< HEAD
Crash: e_closure: failed to resolve capture value
---
Crash: e_closure: failed to resolve capture value
---
Crash: e_closure: failed to resolve capture value
---
Crash: e_closure: failed to resolve capture value
---
Crash: e_closure: failed to resolve capture value
||||||| 719b878da4
Evaluation error: error.BugUnboxedRigidVar
---
Evaluation error: error.BugUnboxedRigidVar
---
Evaluation error: error.BugUnboxedRigidVar
---
Evaluation error: error.MethodLookupFailed
---
Evaluation error: error.MethodLookupFailed
=======
False
>>>>>>> origin/main
# PROBLEMS
NIL
