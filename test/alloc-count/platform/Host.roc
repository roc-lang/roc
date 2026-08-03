## Hosted functions provided by the alloc-count test host.
Host := [].{
    ## Number of allocation calls (roc_alloc plus roc_realloc) the host has
    ## served for the running app so far. Read it before and after a region of
    ## code and subtract to count that region's allocations.
    alloc_count! : () => U64
}
