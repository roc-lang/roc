# Utils module - base utilities with no dependencies
Utils := [].{
    # Simple tag function
    tag : Str -> Str
    tag = |s| "<${s}>"

    # Simple quote function
    quote : Str -> Str
    quote = |s| "\"${s}\""
}
