## Large constant tables of tag unions whose payloads are all zero-sized,
## which another module reads.
Tags := [North, East, South, West].{
    build : U64, (U64 -> a) -> List(a)
    build = |len, item| {
        var $out = List.with_capacity(len)
        var $index = 0.U64
        while $index < len {
            $out = List.append($out, item($index))
            $index = $index + 1
        }
        $out
    }

    ## An evaluated table of this nominal type's tags.
    headings : List(Tags)
    headings = Tags.build(3000, |index| if index % 4 == 0 { Tags.North } else if index % 4 == 1 { Tags.East } else if index % 3 == 0 { Tags.South } else { Tags.West })

    ## An evaluated table of `Bool`s.
    flags : List(Bool)
    flags = Tags.build(5000, |index| index % 3 == 0)

    ## A literal table of a structural union's tags.
    lights : List([Red, Amber, Green])
    lights = [Red, Amber, Green, Green, Red, Amber, Red]

    ## An evaluated table of records holding tags.
    cells : List({ alive : Bool, age : U8, light : [Red, Amber, Green] })
    cells = Tags.build(2000, |index| { alive: index % 5 != 0, age: (index % 200).to_u8_wrap(), light: if index % 2 == 0 { Red } else { Green } })

    ## An evaluated table of tags whose payloads are zero-sized.
    marks : List([Hit({}), Miss, Skip({}, [Only])])
    marks = Tags.build(3000, |index| if index % 3 == 0 { Hit({}) } else if index % 3 == 1 { Miss } else { Skip({}, Only) })

    weight : Tags -> U64
    weight = |heading| match heading {
        North => 1
        East => 2
        South => 3
        West => 4
    }
}
