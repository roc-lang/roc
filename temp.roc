module []

expect

    _ =
        when new_scatter { data : []} is
            Ok asdf -> scatter_to_str asdf
            Err _ -> crash ""

    1 == 1


Trace x := {
    data : List { x : x},
    orientation : [Vertical],
    name : Str,
    marker : Marker,
}
    implements [Inspect]

new_scatter :
    {
        data : List { x : x },
        orientation ? [Vertical],
        name ? Str,
    }
    -> Result (Trace x) _
new_scatter = \{ data, orientation ? Vertical, name ? ""} ->
    Ok
        (
            @Trace {
                data,
                orientation,
                name,
                marker: new_marker? {},
            }
        )

# CHANING ANYHTING IN HERE SEEMS TO "FIX" IT
scatter_to_str : Trace x -> Str where x implements Inspect
scatter_to_str = \@Trace inner ->

    # NOT USED ... BUT WE CAN"T REMOVE, OR BUG GOES AWAY??
    data2 = List.walk inner.data ([]) \(xs), { x } -> (List.append xs x)

    # NOT USED ... BUT WE CAN"T REMOVE, OR BUG GOES AWAY??
    orientation_str = if inner.orientation == Vertical then "\"orientation\":\"v\"" else "\"orientation\":\"h\""

    # NOT USED ... BUT WE CAN"T REMOVE, OR BUG GOES AWAY??
    name_str = if Str.is_empty inner.name then "" else "\"name\":\"$(inner.name)\""

    # NOT USED ... BUT WE CAN"T REMOVE, OR BUG GOES AWAY??
    marker_str = marker_to_str inner.marker #"testtt"

    ""


Marker := {}
    implements [Inspect]

new_marker : {} -> Result Marker _
new_marker = \{} -> Ok (@Marker {})

marker_to_str : Marker -> Str
marker_to_str = \_ -> ""
