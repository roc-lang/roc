# One case of each construct the generator composes, with values chosen to agree on every config.
gen.fixture(
    [
        gen.Nom("Tree", "derived", ("tags", [("Leaf", []), ("Node", [("self",), ("int", "U8"), ("self",)])])),
        gen.Nom("Tok", "custom", ("record", [("v", ("str",), False)]), opaque=True),
        gen.Nom("Box1", "derived", ("tags", [("Has", [("param",)]), ("Nope", [])]), param=True),
    ],
    [
        (("record", [("a", ("str",), False), ("b", ("int", "I8"), True), ("c", ("dec",), False)]),
         ("rec", {"a": "q\"\\\n\x01é\U0001F600", "b": ("tag", "Err", [("tag", "Missing", [])]), "c": -10**18 // 4}),
         ("rec", {"a": "q\"\\\n\x01é\U0001F600", "b": ("tag", "Ok", [-128]), "c": -10**18 // 4})),
        (("tags", [("A", []), ("B", [("unit",)]), ("C", [("int", "U128"), ("bool",)])]),
         ("tag", "C", [2**128 - 1, True]), ("tag", "C", [2**128 - 1, True])),
        (("try", ("list", ("int", "I64")), ("tags", [("Bad", [("str",)])])),
         ("tag", "Ok", [[-2**63, 2**63 - 1, 0]]), ("tag", "Err", [("tag", "Bad", ["x"])])),
        (("list", ("try", ("str",), ("tags", [("Null", [])]))),
         [("tag", "Ok", ["a"]), ("tag", "Err", [("tag", "Null", [])])], [("tag", "Ok", ["a"]), ("tag", "Err", [("tag", "Null", [])])]),
        (("nom", "Tree", []),
         ("tag", "Node", [("tag", "Leaf", []), 7, ("tag", "Node", [("tag", "Leaf", []), 255, ("tag", "Leaf", [])])]),
         ("tag", "Node", [("tag", "Leaf", []), 7, ("tag", "Leaf", [])])),
        (("tuple", [("nom", "Tok", []), ("nom", "Box1", [("int", "U16")]), ("set", ("str",))]),
         (("rec", {"v": "tok"}), ("tag", "Has", [65535]), ("set", ["x", "y"])),
         (("rec", {"v": "tok"}), ("tag", "Has", [65535]), ("set", ["y", "x"]))),
    ],
)
