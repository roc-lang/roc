app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Regression test: calling a builtin method on args whose type
# is only resolved via platform requirements used to panic with:
#   "resolveUnresolvedTypeVarRequirement: no candidate for member 'drop_first'"
# The deferred requirement was lost because checkPlatformRequirements
# used a local env that was released without processing deferred constraints.
main! = |args| {
    _rest = args.drop_first(1)
    Stdout.line!("ok")
    Ok({})
}
