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

; Each closure, wrapped so that its arrival says which one it was.
(proc indexed (f i) eff
  (let v (callc! f))
  (ret (tup i v)))

(proc index_all (fs i n acc) pure
  (let c (lt i n))
  (if c
    (block
      (let f (get fs i))
      (let w (clo indexed f i))
      (let acc2 (push acc w))
      (let i2 (add i 1))
      (tail index_all fs i2 n acc2))
    (block (ret acc))))

; ------------------------------------------------------------- Concurrent
(proc all! (fs) eff
  (let n (len fs))
  (let ws (call index_all fs 0 n (list)))
  (let p (call concurrently ws))
  (let res (call zeros n (list)))
  (let g (group p))
  (tail all_loop g res))

(proc all_loop (g res) eff
  (let a (next g))
  (match a
    (Arrived (iv) (block
      (let i (field iv 0))
      (let v (field iv 1))
      (let res2 (set res i v))
      (tail all_loop g res2)))
    (Cancelled () (block (let u (str "unreachable: a loop body saw Cancelled")) (crash u)))
    (Ended () (block (ret res)))))

(proc both! (f g) eff
  (let fs (list f g))
  (let res (call all! fs))
  (let a (get res 0))
  (let b (get res 1))
  (ret (tup a b)))

(proc all_ok! (fs) eff
  (let n (len fs))
  (let ws (call index_all fs 0 n (list)))
  (let p (call concurrently ws))
  (let res (call zeros n (list)))
  (let g (group p))
  (tail ok_loop g res))

(proc ok_loop (g res) eff
  (let a (next g))
  (match a
    (Arrived (iv) (block
      (let i (field iv 0))
      (let v (field iv 1))
      (match v
        (Ok (x) (block
          (let res2 (set res i x))
          (tail ok_loop g res2)))
        (Err (e) (block
          (do (call close_loop g))
          (ret (tag Err e)))))))
    (Cancelled () (block (let u (str "unreachable: a loop body saw Cancelled")) (crash u)))
    (Ended () (block (ret (tag Ok res))))))

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
COMBINATORS = ("all!", "both!", "all_ok!", "race!", "supervise!")
