## Host module with an opaque nominal type containing data and hosted effects.
I64ToI64 : I64 -> I64

Host :: {
    name : Str,
}.{
    Tree := [Leaf(I64), Node(Box(Tree), Box(Tree))]

    ## Return a boxed host-provided function with a primitive capture.
    boxed_add! : I64 => Box(I64ToI64)

    ## Return a string summarizing how many boxed host captures have been dropped.
    boxed_drop_report! : () => Str

    ## Return a boxed host-provided function with a nested record capture.
    boxed_nested_record! : Str => Box(I64ToI64)

    ## Return a boxed host-provided function that captures a recursive tag union.
    boxed_recursive_tree! : Tree => Box(I64ToI64)

    ## Return a boxed host-provided function whose capture owns another boxed function.
    boxed_with_boxed_capture! : Box(I64ToI64), I64 => Box(I64ToI64)

    ## Call a boxed function from the host using the erased callable ABI.
    call_boxed! : Box(I64ToI64), I64 => I64

    ## Create a new Host with the given name
    new : Str -> Host
    new = |n| { name: n }

    ## Get the host's name (pure method for testing)
    get_name : Host -> Str
    get_name = |host| host.name

    ## Get a greeting - this is a hosted effect that takes Host as first argument
    get_greeting! : Host => Str

    ## Release the boxed function currently stored by the host.
    release_stored_boxed! : () => {}

    ## Reset boxed host capture drop counters.
    reset_boxed_drop_report! : () => {}

    ## Return the same boxed function back to Roc after taking a host reference.
    roundtrip_boxed! : Box(I64ToI64) => Box(I64ToI64)

    ## Store a boxed function in the host by incrementing its outer refcount.
    store_boxed! : Box(I64ToI64) => {}

    ## Call the boxed function previously stored by store_boxed!.
    stored_boxed_call! : I64 => I64
}
