# A generalized self-recursive callable value built by a block, imported by
# RecursiveCounterAlias.roc. Each importer specialization is evaluated in the
# importing program, so its stored closure captures this module's recursive
# binding.
RecursiveCounter := [].{
    count : U64 -> [Done, ..]
    count = {
        z = 0
        |n| if n == z { Done } else { count(n - 1) }
    }
}
