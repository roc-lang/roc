app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br" }

main =
    # Test case 1: Simple heterogeneous list
    list1 = [1, "hello", 3.14]

    # Test case 2: Nested heterogeneous list - inner list has type mismatch
    list2 = [[1, "hello"], [2, 3]]

    # Test case 3: Deeply nested heterogeneous list
    list3 = [[[1, 2], [3, 4]], [["a", "b"], ["c", "d"]]]

    # Test case 4: Mixed heterogeneous issues
    list4 = [[1, 2], ["hello", "world"], [3.14, 2.71]]

    Stdout.line! "If this compiles, something is wrong!"
