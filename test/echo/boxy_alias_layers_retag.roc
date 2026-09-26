# Boxy describes an alias by the first representation under its alias layers,
# and steps through nothing else. `Outer : Inner` is two alias layers over a
# zero-sized row: used where its result row stays open, a descriptor built
# from either layer's own erased storage would list none of the row's tags,
# and the re-tag into `[Aborted, Other]` would read `Aborted`, which sorts
# first. `Shade : Color` and `Held : Box(Shade)` are aliases over a
# transparent nominal and over a box, which keep their own descriptors.
Inner : [Other]
Outer : Inner

fwd : Outer -> Outer
fwd = |x| x

annotated : [Other] -> [Aborted, Other]
annotated = |t| fwd(t)

unannotated = |t| fwd(t)

show : [Aborted, Other] -> Str
show = |v| match v { Aborted => "Aborted", Other => "Other" }

Color := [Red, Green(Str)]
Shade : Color

keep : a -> a
keep = |x| x

pick : Shade -> Shade
pick = |c| keep(c)

show_color : Color -> Str
show_color = |c| match c { Red => "Red", Green(s) => "Green ${s}" }

Held : Box(Shade)

hold : Shade -> Held
hold = |c| keep(Box.box(c))

release : Held -> Shade
release = |h| Box.unbox(keep(h))

main! = |_args| {
    echo!("${show(annotated(Other))} ${show(unannotated(Other))}\n")
    echo!("${show_color(pick(Green("leaf")))} ${show_color(release(hold(Red)))} ${Str.inspect(keep(pick(Green("x"))))}\n")
    Ok({})
}
