# A forwarder whose declared result is a zero-sized alias row (`Base : [Other]`),
# used where its result row stays open, so the call's result is an alias over
# a dynamic row. Boxy describes that alias by its backing row: a descriptor
# built from the alias's own erased storage would list none of the row's tags,
# and the re-tag into `[Aborted, Other]` would read `Aborted`, which sorts first.
Base : [Other]

fwd : Base -> Base
fwd = |x| x

F : Base -> Base

merged : F
merged = |x| x

unannotated = |t| fwd(t)

annotated : [Other] -> [Aborted, Other]
annotated = |t| fwd(t)

through_merged : [Other] -> [Aborted, Other]
through_merged = |t| merged(t)

show : [Aborted, Other] -> Str
show = |v| match v { Aborted => "Aborted", Other => "Other" }

main! = |_args| {
    echo!("${show(unannotated(Other))} ${show(annotated(Other))} ${show(through_merged(Other))}\n")
    Ok({})
}
