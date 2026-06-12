## Hosted functions provided by the shared library's embedded host code.
Host := [].{
    ## Double a number in the host.
    double! : I64 => I64

    ## Hosted function that the platform exposes but this app never calls.
    unused_niche_feature! : I64 => I64
}
