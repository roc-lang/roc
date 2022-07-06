app "quicksort"
    packages { pf: "./platform" }
    imports [ foo.Bar.Baz ]
    provides [ quicksort ] to pf
