## An extra exposed type module on top of parser_v1 — a MINOR change.

Extras := { label : Str }.{

    ## Wrap a label.
    from_label : Str -> Extras
    from_label = |label|
        { label: label }

    ## Read the label back.
    to_label : Extras -> Str
    to_label = |extras|
        extras.label
}

expect Extras.to_label(Extras.from_label("hi")) == "hi"
