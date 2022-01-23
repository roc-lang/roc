app "test"
    packages { pf: "./platform" }
    imports [ foo.Bar.Baz ]
    provides [ quicksort ] { Flags, Model, } to pf
