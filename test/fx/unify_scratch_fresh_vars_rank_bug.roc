app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# This is a minimal reproducer for a bug where the unify_scratch.fresh_vars
# list was not being cleared between unify calls. This caused fresh vars
# created at rank 2 (inside a lambda) during processing of f1 to persist
# and then cause a panic when processing f2's annotation at rank 1.
#
# The key pattern is:
# 1. main! has a lambda body (rank pushes to 2)
# 2. Inside that lambda, we look up run! which triggers checkDef(run!)
# 3. run! also has a lambda body (sub_env rank pushes to 2)
# 4. Inside run!'s lambda, we look up f1 and f2 which both have Try(_, _)
# 5. The underscore wildcard in Try annotations caused fresh vars at rank 2
# 6. When processing f2, the fresh_vars from f1 were still in the list

main! = || {
    match run!() {
        Ok(_) => Stdout.line!("ok")
        Err(_) => {}
    }
}

run! = || {
    _x = f1(0)?
    _y = f2(0)?
    Ok({})
}

f1 : U64 -> Try(U64, _)
f1 = |_| Ok(0)

f2 : U64 -> Try(U64, _)
f2 = |_| Ok(0)
