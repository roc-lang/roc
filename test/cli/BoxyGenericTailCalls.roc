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

# A function with a concrete type that reaches itself through a generic one
# has its result converted between the two representations on the way back.
concrete_through_apply : U64 -> U64
concrete_through_apply = |n| if n == 0 0 else apply(|m| concrete_through_apply(m - 1), n)

# A generic function used as a value at a concrete type is wrapped in an
# adapter that converts its result.
call_with : ((U64 -> U64), U64 -> U64), U64 -> U64
call_with = |g, x| g(|j| through_generic_value(j), x)

through_generic_value : U64 -> U64
through_generic_value = |n| if n == 0 0 else call_with(apply, n - 1)

# The converted result is not always a number: whoever makes the last call
# of the cycle stores its result as the concrete type each function returns.
label_through_apply : U64, Str -> Str
label_through_apply = |n, label| if n == 0 Str.concat(label, "!") else apply(|m| label_through_apply(m - 1, label), n)

pair_through_apply : U64, Str -> { count : U64, label : Str }
pair_through_apply = |n, label| if n == 0 { count: 7, label } else apply(|m| pair_through_apply(m - 1, label), n)

list_through_apply : U64, List(Str) -> List(Str)
list_through_apply = |n, items| if n == 0 items else apply(|m| list_through_apply(m - 1, items), n)

# A generic result that is not a bare type variable keeps the descriptor it
# arrives with when it is converted.
keep_list : U64, List(a) -> List(a)
keep_list = |n, items| if n == 0 items else apply(|m| keep_list(m - 1, items), n)

keep_pair : U64, (a, Str) -> (a, Str)
keep_pair = |n, pair| if n == 0 pair else apply(|m| keep_pair(m - 1, pair), n)

expect ping(20_000, wider(1), "done") == "done"
expect countdown(20_000, wider(1), "done") == "done"
expect through_apply(30_000, "done") == "done"
expect through_lambda_calling_generic(30_000) == 0
expect concrete_through_apply(30_000) == 0
expect through_generic_value(30_000) == 0
expect label_through_apply(30_000, "a label long enough to live on the heap") == "a label long enough to live on the heap!"
expect pair_through_apply(30_000, "a label long enough to live on the heap") == { count: 7, label: "a label long enough to live on the heap" }
expect list_through_apply(30_000, ["one", "two"]) == ["one", "two"]
expect keep_list(30_000, ["one", "two"]) == ["one", "two"]
expect keep_pair(30_000, (1.U8, "two")) == (1.U8, "two")
