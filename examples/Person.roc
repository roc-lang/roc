interface Person
    exposes [
        new,
    ]
    imports [
        Inspect.{ Formatter, Inspector, Inspect },
    ]

Person := {
    firstName : Str,
    lastName : Str,
    age : U8,
    data: Dict Str [Stuff, Thing I32],
    hasBeard: Bool,
}
     has [
         Inspect {
             toInspector: inspectPerson,
         },
     ]

new = @Person

inspectPerson : Person -> Inspector f | f has Formatter
inspectPerson = \@Person { firstName, lastName, age, data, hasBeard } ->
    # In practice, this would never be done manually due to autoderive.
    # Instead you would just write:
    #     Inspect.inspect innerRecord
    # This is what the auto-derive would generate.

    f0 <- Inspect.custom
    [
        { key: "firstName", value: Inspect.str firstName },
        { key: "lastName", value: Inspect.str lastName },
        { key: "age", value: Inspect.u8 age },
        { key: "hasBeard", value: Inspect.bool hasBeard },
        { 
            key: "data",
            value: Inspect.dict data Dict.walk Inspect.str \value ->
                when value is
                    Thing i -> Inspect.tag "Thing" [Inspect.i32 i]
                    Stuff -> Inspect.tag "Stuff" []
                ,
        },
    ]
    |> Inspect.record
    |> Inspect.apply f0
