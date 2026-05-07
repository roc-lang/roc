## Host module with an opaque nominal type containing data and hosted effects.
Tree := [Leaf(I64), Node(Box(Tree), Box(Tree))]

Host :: {
    name : Str,
}.{
    ## Return a boxed host-provided function with a primitive capture.
    boxed_add! : I64 => Box((I64 -> I64))

    ## Return a string summarizing how many boxed host captures have been dropped.
    boxed_drop_report! : () => Str

    ## Return a boxed host-provided function with a nested record capture.
    boxed_nested_record! : Str => Box((I64 -> I64))

    ## Return a boxed host-provided function that captures a recursive tag union.
    boxed_recursive_tree! : Tree => Box((I64 -> I64))

    ## Create a new Host with the given name
    new : Str -> Host
    new = |n| { name: n }

    ## Get the host's name (pure method for testing)
    get_name : Host -> Str
    get_name = |host| host.name

    ## Get a greeting - this is a hosted effect that takes Host as first argument
    get_greeting! : Host => Str

    ## Reset boxed host capture drop counters.
    reset_boxed_drop_report! : () => {}
}
