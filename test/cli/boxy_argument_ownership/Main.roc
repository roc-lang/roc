import GenericFold

Main := []

expect GenericFold.append_all([], [1.U64, 2, 3]) == [1, 2, 3]

expect {
    old = ["a heap allocated string retained by the original list"]
    next = GenericFold.append_all(old, ["another heap allocated string"])
    old == ["a heap allocated string retained by the original list"] and next.len() == 2
}

expect GenericFold.discard_field(["first long heap allocated string"], "second long heap allocated string", ["discarded long heap allocated string"]) == ["first long heap allocated string", "second long heap allocated string"]

expect {
    old = ["a long string shared by both argument fields"]
    (changed, unchanged) = GenericFold.duplicate_fields(old, "another long string appended to one field")
    changed.len() == 2 and unchanged == old and old.len() == 1
}

expect GenericFold.append_deferred(["first long heap allocated string"], ["second long heap allocated string", "third long heap allocated string"]).len() == 3

expect GenericFold.append_all([], [{}, {}, {}]).len() == 3

expect match GenericFold.append_deferred([], [Box.box([1.U64, 2]), Box.box([3.U64])]) {
    [first, second] => Box.unbox(first) == [1, 2] and Box.unbox(second) == [3]
    _ => False
}

# Capturing the outer parameter keeps this a generalized local procedure.
# Its calls pass a leading table word at two distinct instantiations.
expect {
    check_local = |append_item| {
        append = |xs, x| if append_item { xs.append(x) } else { xs }
        numbers = append([1.U64], 2.U64)
        strings = append(["first heap allocated local argument"], "second heap allocated local argument")
        if append_item {
            numbers == [1, 2] and strings == ["first heap allocated local argument", "second heap allocated local argument"]
        } else {
            numbers == [1] and strings == ["first heap allocated local argument"]
        }
    }
    check_local(True) and check_local(False)
}

# The forwarded callable retains a capture while consuming its packed fields.
expect {
    marker = "a heap allocated string held by the forwarded closure"
    initial = ["a heap allocated string retained by the caller"]
    result = GenericFold.forward(|xs, x| xs.append(x).append(marker), initial, "a heap allocated forwarded argument")
    initial.len() == 1 and result == initial.append("a heap allocated forwarded argument").append(marker)
}
