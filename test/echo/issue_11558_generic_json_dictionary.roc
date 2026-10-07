# repro for https://github.com/roc-lang/roc/issues/11558
#
# A generic function that encodes a container of its own type variable asks
# for a dictionary whose nested dictionary is the function's own, so
# `--specialize=no` builds it at runtime from that function's frame.
to_json = |a| Json.to_str([a])
nested_json = |a| Json.to_str([[a], []])
record_json = |a| Json.to_str({ items: [a] })
pair_json = |a, b| Json.to_str([(a, b)])
forward = |a| to_json(a)

main! = |_args| {
    echo!("${to_json("x")}\n")
    echo!("${to_json(1.U8)}\n")
    echo!("${nested_json("y")}\n")
    echo!("${record_json("z")}\n")
    echo!("${pair_json("p", 2.U8)}\n")
    echo!("${forward(["q"])}\n")
    Ok({})
}
