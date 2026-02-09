## Host module with an opaque nominal type containing data and hosted effects.
Host :: {
    name : Str,
}.{
    ## Create a new Host with the given name
    new : Str -> Host
    new = |n| { name: n }

    ## Get the host's name (pure method for testing)
    get_name : Host -> Str
    get_name = |host| host.name

    ## Get a greeting - this is a hosted effect that takes Host as first argument
    get_greeting! : Host => Str
}
