module [
    Community,
    empty,
    addPerson,
    addFriend,
    Person,
    walkFriendNames,
]

## Datatype representing a community for demonstration purposes in inspect-gui.roc and inspect-logging.roc

Community := {
    people : List Person,
    friends : List (Set U64),
}
    implements [Inspect]

Person := {
    firstName : Str,
    lastName : Str,
    age : U8,
    hasBeard : Bool,
    favoriteColor : Color,
}
    implements [Inspect]

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
        List.walk friends (s0, 0) \(s1, id), friendSet ->
            (@Person person) =
                when List.get people id is
                    Ok v -> v
                    Err _ -> crash "Unknown Person"
            personName =
                person.firstName
                |> Str.concat " "
                |> Str.concat person.lastName

            friendNames =
                Set.walk friendSet (Set.empty {}) \friendsSet, friendId ->
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
