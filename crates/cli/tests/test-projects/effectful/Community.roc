module [
    Community,
    empty,
    add_person,
    add_friend,
    Person,
    walk_friend_names,
]

## Datatype representing a community for demonstration purposes in inspect_logging.roc

Community := {
    people : List Person,
    friends : List (Set U64),
}
    implements [Inspect]

Person := {
    first_name : Str,
    last_name : Str,
    age : U8,
    has_beard : Bool,
    favorite_color : Color,
}
    implements [Inspect]

Color : [
    Red,
    Green,
    Blue,
    RGB (U8, U8, U8),
]

empty = @Community({ people: [], friends: [] })

add_person = \@Community({ people, friends }), person ->
    @Community({
        people: List.append(people, @Person(person)),
        friends: List.append(friends, Set.empty({})),
    })

add_friend = \@Community({ people, friends }), from, to ->
    when (List.get(friends, from), List.get(friends, to)) is
        (Ok(from_set), Ok(to_set)) ->
            @Community({
                people,
                friends: friends
                |> List.set(from, Set.insert(from_set, to))
                |> List.set(to, Set.insert(to_set, from)),
            })

        _ ->
            @Community({ people, friends })

walk_friend_names : Community, state, (state, Str, Set Str -> state) -> state
walk_friend_names = \@Community({ people, friends }), s0, next_fn ->
    (out, _) =
        List.walk(friends, (s0, 0), \(s1, id), friend_set ->
            @Person(person) =
                when List.get(people, id) is
                    Ok(v) -> v
                    Err(_) -> crash("Unknown Person")
            person_name =
                person.first_name
                |> Str.concat(" ")
                |> Str.concat(person.last_name)

            friend_names =
                Set.walk(friend_set, Set.empty({}), \friends_set, friend_id ->
                    @Person(friend) =
                        when List.get(people, friend_id) is
                            Ok(v) -> v
                            Err(_) -> crash("Unknown Person")
                    friend_name =
                        friend.first_name
                        |> Str.concat(" ")
                        |> Str.concat(friend.last_name)
                    Set.insert(friends_set, friend_name))

            (next_fn(s1, person_name, friend_names), id + 1))

    out
