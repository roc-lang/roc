# #13429: derived equality of a record holding a recursive nominal panics in Monotype under lambda-set
# specialization (seed 14 of the first fuzz runs, the panicking case on its own).
gen.fixture(
    [gen.Nom("N0", "derived", ("tags", [("Leaf", [("int", "I8")]), ("Node", [("str",), ("self",), ("self",)])]),
             opaque=True)],
    [
        (("record", [("id", ("nom", "N0", []), False), ("v2", ("int", "U128"), False),
                     ("zeta", ("try", ("int", "U8"), ("tags", [("Null", [])])), False)]),
         ("rec", {"id": ("tag", "Leaf", [-50]), "v2": 0, "zeta": ("tag", "Ok", [160])}),
         ("rec", {"id": ("tag", "Node", ["a", ("tag", "Leaf", [-52]), ("tag", "Leaf", [111])]), "v2": 0,
                  "zeta": ("tag", "Ok", [160])})),
    ],
)
