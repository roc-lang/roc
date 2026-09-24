Program(state) := { value : state }.{
    make : state -> Program(state)
    make = |value| Program.({ value: value })
}
