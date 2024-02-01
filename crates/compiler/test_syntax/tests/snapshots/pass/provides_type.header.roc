app "test"
    packages { pf: "./platform" }
    provides [ quicksort ] { Flags, Model, } to pf
