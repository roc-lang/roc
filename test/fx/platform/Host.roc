## Host module with an opaque nominal type containing data and hosted effects.
I64ToI64 : I64 -> I64
UnitToI64 : {} -> I64
UnitToBoxedUnitToI64 : {} -> Box(UnitToI64)

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

    ## Call a boxed function returning another boxed function while declining reuse.
    call_boxed_transition! : Box(UnitToBoxedUnitToI64) => I64

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

    ## Return a host callable that consumes the fifth ABI argument when called.
    boxed_transition! : I64 => Box(UnitToBoxedUnitToI64)

    ## Store any boxed Roc value in the host, which owns it until `take_seed!`
    ## hands it back. The host never inspects it, so its type survives only in
    ## Roc's type system, the way a platform's opaque host-owned values do.
    store_seed! : Box(a) => {}

    ## Return the boxed value `store_seed!` was given. The result type is the
    ## only place `a` appears, so nothing at a call site constrains it.
    take_seed! : () => Box(a)

    ## Store a boxed function in the host by incrementing its outer refcount.
    store_boxed! : Box(I64ToI64) => {}

    ## Call the boxed function previously stored by store_boxed!.
    stored_boxed_call! : I64 => I64

    ## Total the UTF-8 byte lengths of a list of strings. The host owns the
    ## list argument and releases exactly one ownership unit of it, elements
    ## included, before returning.
    sum_str_bytes! : List(Str) => U64
}
