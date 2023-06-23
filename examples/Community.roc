interface Community
    exposes [
        Community,
        empty,
        addPerson,
        addFriend,
        Person,
        walkFriendNames,
    ]
    imports [
        Inspect.{ Formatter, Inspector, Inspect },
    ]

Community := {
    people : List Person,
    friends : List (Set Nat),
}
     has [
         Inspect {
             toInspector: inspectCommunity,
         },
     ]

Person := {
    firstName : Str,
    lastName : Str,
    age : U8,
    hasBeard : Bool,
    favoriteColor: Color,
}
     has [
         Inspect {
             toInspector: inspectPerson,
         },
     ]

Color : [
    Red,
    Green,
    Blue,
    RGB (U8, U8, U8),
]

empty = @Community { people: [], friends: [] }

addPerson = \@Community { people, friends }, person ->
    @Community {
        people: List.append people (@Person person),
        friends: List.append friends (Set.empty {}),
    }

addFriend = \@Community { people, friends }, from, to ->
    when (List.get friends from, List.get friends to) is
        (Ok fromSet, Ok toSet) ->
            @Community {
                people,
                friends: friends
                |> List.set from (Set.insert fromSet to)
                |> List.set to (Set.insert toSet from),
            }

        _ ->
            @Community { people, friends }

walkFriendNames : Community, state, (state, Str, Set Str -> state) -> state
walkFriendNames = \@Community { people, friends }, s0, nextFn ->
    (out, _) =
        (s1, id), friendSet <- List.walk friends (s0, 0)
        (@Person person) =
            when List.get people id is
                Ok v -> v
                Err _ -> crash "Unknown Person"
        personName =
            person.firstName
            |> Str.concat " "
            |> Str.concat person.lastName

        friendNames =
            friendsSet, friendId <- Set.walk friendSet (Set.empty {})
            (@Person friend) =
                when List.get people friendId is
                    Ok v -> v
                    Err _ -> crash "Unknown Person"
            friendName =
                friend.firstName
                |> Str.concat " "
                |> Str.concat friend.lastName
            Set.insert friendsSet friendName

        (nextFn s1 personName friendNames, id + 1)
    out

inspectCommunity : Community -> Inspector f | f has Formatter
inspectCommunity = \@Community { people, friends } ->
    f0 <- Inspect.custom
    [
        { key: "people", value: Inspect.list people List.walk Inspect.toInspector },
        {
            key: "friends",
            value: Inspect.list
                friends
                List.walk
                (\s -> Inspect.set
                        s
                        Set.walk
                        (\num -> num |> Num.toU64 |> Inspect.u64)
                ),
            # value: Inspect.dict
            #     (@Community { people, friends })
            #     walkFriendNames
            #     Inspect.str
            #     (\s -> Inspect.set s Set.walk Inspect.str),
        },
    ]
    |> Inspect.record
    |> Inspect.apply f0

inspectPerson : Person -> Inspector f | f has Formatter
inspectPerson = \@Person { firstName, lastName, age, hasBeard, favoriteColor } ->
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
            key: "favoriteColor",
            value:
                when favoriteColor is
                    Red ->
                        Inspect.tag "Red" []
                    Green ->
                        Inspect.tag "Green" []
                    Blue ->
                        Inspect.tag "Blue" []
                    RGB (r, g, b) ->
                        Inspect.tag "RGB" [Inspect.tuple [Inspect.u8 r, Inspect.u8 g, Inspect.u8 b]]
                ,
        },
    ]
    |> Inspect.record
    |> Inspect.apply f0
