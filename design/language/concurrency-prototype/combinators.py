"""The builtins: plans, the loop exit, and the `Concurrent` library.

Plans are inert values built by pure functions (plans.py). A concurrent loop

    for! arrival in plan { body }

is lowered to explicit operations, the way the compiler lowers it:

    g = group(plan)                     enter: start the running set
    loop:
      match next(g)                     the loop head, a suspension site
        Arrived(v)  => body             any exit of the body runs close_loop(g)
        Ended       => after the loop   every entry has arrived

An entry's crash never arrives: it crashes the caller at once, wherever the
caller waits (runtime.py, `linked_crash`), so the body never sees one.
`close_loop` is the one exit sequence: cancel what may be cancelled, then wait
at the loop head until every entry still running has finished. The body never
sees `Cancelled`; only `close_loop` does.

`all!`, `both!`, `all_ok!` and `race!` are ordinary library functions written
with loops. `supervise!` is the one place that reacts to a crash, so it stays
on the runtime's internal operations (`spawn`, `await`), whose `Crashed`
report programs cannot reach.

The same code gives the concurrent behavior in `Tasks` builds and the
sequential behavior in `None` builds, because only the meaning of the loop
operations changes between them.
"""

from ir import parse

SOURCE = r"""
; ------------------------------------------------------------- plans (pure)
(proc concurrently (fs) pure (ret (tag Each fs)))

(proc concat (p q) pure (ret (tag Cat p q)))

(proc limit (p n) pure
  (let z (lt n 1))
  (if z
    (block (let u (str "Concurrently.limit: the limit must be positive")) (crash u))
    (block (ret (tag Limit n p)))))

(proc shielded (p) pure (ret (tag Shield p)))

(proc background (p) pure (ret (tag Background p)))

; ------------------------------------------------------------- loop exit
(proc close_loop (g) eff
  (do (cancel_group g))
  (tail close_wait g))

(proc close_wait (g) eff
  (let a (next g))
  (match a
    (Arrived (v) (block (tail close_wait g)))
    (Cancelled () (block (tail close_wait g)))
    (Ended () (block (ret 0)))))

; ------------------------------------------------------------- helpers
; A list of n zeros, to hold results by index.
(proc zeros (n acc) pure
  (let z (eq n 0))
  (if z
    (block (ret acc))
    (block
      (let acc2 (push acc 0))
      (let n2 (sub n 1))
      (tail zeros n2 acc2))))

; Each closure, wrapped so that its arrival says which one it was: At(i, v),
; or Skip for an entry of a background plan, whose result is left out.
(proc indexed (f i) eff
  (let v (callc! f))
  (ret (tag At i v)))

(proc skipped (f) eff
  (let v (callc! f))
  (ret (tag Skip)))

; The closures of fs from index k, wrapped with indices from i on.
(proc index_each (fs k n i acc) pure
  (let c (lt k n))
  (if c
    (block
      (let f (get fs k))
      (let w (clo indexed f i))
      (let acc2 (push acc w))
      (let k2 (add k 1))
      (let i2 (add i 1))
      (tail index_each fs k2 n i2 acc2))
    (block (ret acc))))

(proc skip_each (fs k n acc) pure
  (let c (lt k n))
  (if c
    (block
      (let f (get fs k))
      (let w (clo skipped f))
      (let acc2 (push acc w))
      (let k2 (add k 1))
      (tail skip_each fs k2 n acc2))
    (block (ret acc))))

; The plan p with each entry wrapped: the ones that are not background get
; indices from i on, in input order. Returns the new plan and the next index.
; Limits, shields and background marks keep their places.
(proc index_plan (p i bg) pure
  (match p
    (Each (fs) (block
      (let n (len fs))
      (if bg
        (block
          (let ws (call skip_each fs 0 n (list)))
          (let q (tag Each ws))
          (ret (tup q i)))
        (block
          (let ws (call index_each fs 0 n i (list)))
          (let q (tag Each ws))
          (let i2 (add i n))
          (ret (tup q i2))))))
    (Cat (a b) (block
      (let ra (call index_plan a i bg))
      (let a2 (field ra 0))
      (let i1 (field ra 1))
      (let rb (call index_plan b i1 bg))
      (let b2 (field rb 0))
      (let i2 (field rb 1))
      (let q (tag Cat a2 b2))
      (ret (tup q i2))))
    (Limit (n a) (block
      (let ra (call index_plan a i bg))
      (let a2 (field ra 0))
      (let i2 (field ra 1))
      (let q (tag Limit n a2))
      (ret (tup q i2))))
    (Shield (a) (block
      (let ra (call index_plan a i bg))
      (let a2 (field ra 0))
      (let i2 (field ra 1))
      (let q (tag Shield a2))
      (ret (tup q i2))))
    (Background (a) (block
      (let t (int 1))
      (let ra (call index_plan a i t))
      (let a2 (field ra 0))
      (let i2 (field ra 1))
      (let q (tag Background a2))
      (ret (tup q i2))))))

; ------------------------------------------------------------- plans run whole
; plan.all!() and plan.all_ok!(): results in input order, without those of
; background entries.
(proc plan_all! (p) eff
  (let z (int 0))
  (let r (call index_plan p 0 z))
  (let q (field r 0))
  (let n (field r 1))
  (let res (call zeros n (list)))
  (let g (group q))
  (tail all_loop g res))

(proc all_loop (g res) eff
  (let a (next g))
  (match a
    (Arrived (iv) (block
      (match iv
        (At (i v) (block
          (let res2 (set res i v))
          (tail all_loop g res2)))
        (Skip () (block (tail all_loop g res))))))
    (Cancelled () (block (let u (str "unreachable: a loop body saw Cancelled")) (crash u)))
    (Ended () (block (ret res)))))

(proc plan_all_ok! (p) eff
  (let z (int 0))
  (let r (call index_plan p 0 z))
  (let q (field r 0))
  (let n (field r 1))
  (let res (call zeros n (list)))
  (let g (group q))
  (tail ok_loop g res))

(proc ok_loop (g res) eff
  (let a (next g))
  (match a
    (Arrived (iv) (block
      (match iv
        (At (i v) (block
          (match v
            (Ok (x) (block
              (let res2 (set res i x))
              (tail ok_loop g res2)))
            (Err (e) (block
              (do (call close_loop g))
              (ret (tag Err e)))))))
        (Skip () (block (tail ok_loop g res))))))
    (Cancelled () (block (let u (str "unreachable: a loop body saw Cancelled")) (crash u)))
    (Ended () (block (ret (tag Ok res))))))

; ------------------------------------------------------------- Concurrent
; Concurrent.all!(fs) is fs.concurrently().all!(), and likewise all_ok!.
(proc all! (fs) eff
  (let p (call concurrently fs))
  (tail plan_all! p))

(proc both! (f g) eff
  (let fs (list f g))
  (let res (call all! fs))
  (let a (get res 0))
  (let b (get res 1))
  (ret (tup a b)))

(proc all_ok! (fs) eff
  (let p (call concurrently fs))
  (tail plan_all_ok! p))

(proc race! (f fs) eff
  (let first (list f))
  (let p1 (call concurrently first))
  (let p2 (call concurrently fs))
  (let p (call concat p1 p2))
  (let g (group p))
  (let a (next g))
  (match a
    (Arrived (v) (block
      (do (call close_loop g))
      (ret v)))
    (Cancelled () (block (let u (str "unreachable: a loop body saw Cancelled")) (crash u)))
    (Ended () (block (let u (str "unreachable: race! always has an entrant")) (crash u)))))

; ------------------------------------------------------------- supervise!
; Spawn every closure in fs, from index i; returns the list of handles.
(proc spawn_all (fs i n acc) eff
  (let c (lt i n))
  (if c
    (block
      (let f (get fs i))
      (let h (spawn f))
      (let acc2 (push acc h))
      (let i2 (add i 1))
      (tail spawn_all fs i2 n acc2))
    (block (ret acc))))

(proc supervise! (max f) eff
  (tail sup_loop max f 0))

(proc sup_loop (max f k) eff
  (let fs (list f))
  (let kids (call spawn_all fs 0 1 (list)))
  (let r (await kids))
  (match r
    (Done (i v) (block (ret v)))
    (Crashed (i m) (block
      (let c (lt k max))
      (if c
        (block
          (let k2 (add k 1))
          (tail sup_loop max f k2))
        (block (crash m)))))
    (Cancelled (i) (block (let u (str "unreachable: supervise! saw Cancelled")) (crash u)))))
"""

BUILTINS = parse(SOURCE)
COMBINATORS = ("all!", "both!", "all_ok!", "race!", "supervise!", "plan_all!", "plan_all_ok!")
