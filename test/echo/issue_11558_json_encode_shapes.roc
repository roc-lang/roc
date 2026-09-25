# repro for https://github.com/roc-lang/roc/issues/11558
#
# Json encoding through `--specialize=no` reaches static dictionaries whose
# method workers take variables only their where-clauses mention, record
# fields stored in presence slots, and `List.encoder_for` passing a
# dictionary to its own recursive instantiation. Every shape encodes as it
# does specialized.
main! = |_args| {
    echo!("${Json.to_str(["x"])}\n")
    echo!("${Json.to_str([["a", "b"], []])}\n")
    echo!("${Json.to_str([[["deep"]]])}\n")
    echo!("${Json.to_str([[1, 2], [3]])}\n")
    echo!("${Json.to_str({ names: ["x", "y"], n: 3 })}\n")
    echo!("${Json.to_str([{ a: [1, 2] }])}\n")
    echo!("${Json.to_str({ grid: [["a"], ["b", "c"]] })}\n")
    echo!("${Json.to_str((["x"], [["y"]]))}\n")
    Ok({})
}
