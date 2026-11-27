# #13430: Boxy encodes a tag whose payloads are all zero-sized, in a union of two or more tags, by reading fields
# out of a zero-sized payload. Every --specialize=no path that encodes fails; --specialize=yes agrees.
gen.fixture(
    [],
    [
        (("tags", [("P", []), ("Zed", [("unit",), ("unit",)])]),
         ("tag", "Zed", [("unit",), ("unit",)]), ("tag", "P", [])),
    ],
)
