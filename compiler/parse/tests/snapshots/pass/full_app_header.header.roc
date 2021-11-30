app "quicksort"
    packages { base: "./platform" }
    imports [ foo.Bar.Baz ]
    provides [ quicksort ] to base
