app "quicksort"
    packages { pf: "./platform", }
    imports [ foo.Bar.{
        Baz,
        FortyTwo,
        # I'm a happy comment
    } ]
    provides [ quicksort, ] to pf
