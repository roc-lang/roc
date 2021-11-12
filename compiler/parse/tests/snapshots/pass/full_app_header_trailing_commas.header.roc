app "quicksort"
    packages { base: "./platform", }
    imports [ foo.Bar.{
        Baz,
        FourtyTwo,
        # I'm a happy comment
    } ]
    provides [ quicksort, ] to base
