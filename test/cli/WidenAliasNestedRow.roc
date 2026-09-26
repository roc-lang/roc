# The adapter-reach restriction reached through a
# declaration REFERENCE. `Statuses` defers the row it writes, because an alias
# declaration cannot know where its uses will put it—but the reference here
# stands in `statuses`'s direct result, and the row sits inside a `List`, which
# no adapter can re-tag. So the row is contributed as written and the body use
# that widens it is an ordinary type mismatch at the use.
#
# Before the reference walk became position-aware this program CHECKED and then
# panicked in `unifyTagRows` while lowering, which is why it is pinned as a
# rejection on the CLI path and not only in the checker's own tests.
WidenAliasNestedRow := {}

Statuses : List([Ok(Str), Err(Str)])

describe : a -> List([Ok(Str), Err(Str), Extra]) where [a.statuses : a -> Statuses]
describe = |x| x.statuses()

# `closed_statuses` is deliberately UNANNOTATED, and its row is read out of a nominal
# field. Neither an annotation nor a forwarding function can produce a closed
# row any more: an annotated value's implicitly opened row is quantified
# (design.md "Polarity"), and a top-level function that FORWARDS a closed value
# has its result row coerced open again at every use (design.md "Row
# Subsumption"). A nominal declaration's body closes its rows as written, so a
# field of `Closed` is a closed source that no coercion reopens, and `closed_statuses`
# is a top-level constant whose row is closed, which is what this fixture needs.
Closed := { v : Statuses }

closed_statuses = Closed.{ v: [Ok("cv")] }.v

Job := [Pending].{
    statuses : Job -> Statuses
    statuses = |_| closed_statuses
}

main = describe(Job.Pending)
