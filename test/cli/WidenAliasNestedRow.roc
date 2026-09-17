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

closed_statuses : Statuses
closed_statuses = [Ok("cv")]

Job := [Pending].{
    statuses : Job -> Statuses
    statuses = |_| closed_statuses
}

main = describe(Job.Pending)
