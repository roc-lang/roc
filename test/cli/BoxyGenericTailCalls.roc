BoxyGenericTailCalls := {}

# Without specialization a function that is generic in its result returns a
# type descriptor along with the value, and a call to another generic function
# crosses between the two functions' own type variables. Each cycle below is
# still made only of tail calls. Building a wide record on every call makes each frame large,
# so a cycle that kept its frames would overflow the stack at these depths.

Wide : { a : U64, b : U64, c : U64, d : U64, e : U64, f : U64, g : U64, h : U64, i : U64, j : U64, k : U64, l : U64, m : U64, n : U64, o : U64, p : U64 }

Wider : { a : Wide, b : Wide, c : Wide, d : Wide, e : Wide, f : Wide, g : Wide, h : Wide }

wide : U64 -> Wide
wide = |v| { a: v, b: v, c: v, d: v, e: v, f: v, g: v, h: v, i: v, j: v, k: v, l: v, m: v, n: v, o: v, p: v }

wider : U64 -> Wider
wider = |v| { a: wide(v), b: wide(v), c: wide(v), d: wide(v), e: wide(v), f: wide(v), g: wide(v), h: wide(v) }

ping : U64, Wider, a -> a
ping = |count, w, x| if count == 0 x else pong(count - 1, wider(w.a.a + count), x)

pong : U64, Wider, a -> a
pong = |count, w, x| if count == 0 x else ping(count - 1, wider(w.a.a + count), x)

countdown : U64, Wider, a -> a
countdown = |count, w, x| if count == 0 x else countdown(count - 1, wider(w.a.a + count), x)

apply : (a -> b), a -> b
apply = |f, x| f(x)

through_apply : U64, a -> a
through_apply = |count, x| if count == 0 x else apply(|y| through_apply(count - 1, y), x)

decrement = |n| n - 1

through_lambda_calling_generic = |n| if n == 0 0 else (|m| through_lambda_calling_generic(decrement(m)))(n)

expect ping(20_000, wider(1), "done") == "done"
expect countdown(20_000, wider(1), "done") == "done"
expect through_apply(30_000, "done") == "done"
expect through_lambda_calling_generic(30_000) == 0
