app "quicksort"
    packages { base: "./platform", }
    imports [ foo.Bar.{
        Baz,
        FortyTwo,
        # I'm a happy comment
    } ]
    provides [ quicksort, ] to base
