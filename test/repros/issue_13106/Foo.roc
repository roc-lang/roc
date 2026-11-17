module [Bool]

Bool := [Yes, No].{
    show : Bool -> Str
    show = |b| match b {
        Yes => "yes"
        No => "no"
    }
}
