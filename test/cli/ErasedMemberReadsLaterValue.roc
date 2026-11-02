# Repro for https://github.com/roc-lang/roc/issues/11678 (the panic every
# roc-gui app hits on its first build, cached or not).
#
# `boxed` is a compile-time value whose type holds an erased callable, and the
# list unifies its member set with a closure that reads `ink`. `ink` reads
# `base`, so it completes one evaluation level after `boxed`. Evaluating
# `boxed` must not need `ink`: `boxed`'s own evaluation never creates or calls
# that closure. A member that reads `boxed` itself is no dependency cycle
# either, for the same reason.
ErasedMemberReadsLaterValue := {}

base = 6.U32

ink = base + 1

boxed : Box(U32 -> U32)
boxed = Box.box(|x| x)

expect {
	List.map([boxed, Box.box(|x| x + ink)], |b| Box.unbox(b)(1)) == [1, 8]
}

expect {
	List.map([boxed, Box.box(|x| Box.unbox(boxed)(x) + 1)], |b| Box.unbox(b)(1)) == [1, 2]
}
