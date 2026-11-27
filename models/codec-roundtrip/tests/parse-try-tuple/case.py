# #13431: parsing a builtin Try with a tuple payload panics in Monotype under lambda-set specialization.
gen.fixture([], [(("try", ("tuple", [("int", "I8"), ("int", "I8")]), ("tags", [("Leaf", [])])),
                  ("tag", "Ok", [(1, 2)]), ("tag", "Err", [("tag", "Leaf", [])]))])
