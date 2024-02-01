interface ManualAttr
    exposes []

# manually replicates the Attr wrapping that uniqueness inference uses, to try and find out why they are different
# It is very important that there are no signatures here! elm uses an optimization that leads to less copying when
# signatures are given.

map =
    unAttr = \Attr _ foobar -> foobar

    r = Attr unknown "bar"

    s = Attr unknown2 { left: Attr Shared "foo" }

    when True is
        _ -> { y: r }
        _ -> { y: (unAttr s).left }
