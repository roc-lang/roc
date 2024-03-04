app "test"
    packages { pf: platform "./platform" }
    provides [ quicksort ] { Flags, Model, } to pf
