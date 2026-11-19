"""The `Concurrent` combinators, written in the IR over the three internal
operations, as the RFC specifies ("The `Concurrent` module").

The same code gives the concurrent behavior in `Tasks` builds and the
sequential behavior in `None` builds, because only the meaning of `spawn`,
`await` and `cancel` changes between them.

`await` returns one of
    Done(i, v)      child i of the list finished with v
    Cancelled(i)    child i was cancelled
    Crashed(i, m)   child i crashed with message m
"""

from ir import parse

SOURCE = r"""
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

; A list of n zeros, to hold results by index.
(proc zeros (n acc) pure
  (let z (eq n 0))
  (if z
    (block (ret acc))
    (block
      (let acc2 (push acc 0))
      (let n2 (sub n 1))
      (tail zeros n2 acc2))))

(proc cancel_all (kids i n) eff
  (let c (lt i n))
  (if c
    (block
      (let h (get kids i))
      (do (cancel h))
      (let i2 (add i 1))
      (tail cancel_all kids i2 n))
    (block (ret 0))))

; Await `left` more reports and discard them.
(proc drain (kids left) eff
  (let z (eq left 0))
  (if z
    (block (ret 0))
    (block
      (let r (await kids))
      (let l2 (sub left 1))
      (tail drain kids l2))))

; Cancel the rest, await them, and crash with m: the response to a Crashed
; report in every combinator but supervise!.
(proc fail (kids left m) eff
  (let n (len kids))
  (do (call cancel_all kids 0 n))
  (do (call drain kids left))
  (crash m))

(proc all! (fs) eff
  (let n (len fs))
  (let kids (call spawn_all fs 0 n (list)))
  (let res (call zeros n (list)))
  (tail await_all kids res n))

(proc await_all (kids res left) eff
  (let z (eq left 0))
  (if z
    (block (ret res))
    (block
      (let r (await kids))
      (let l2 (sub left 1))
      (match r
        (Done (i v) (block
          (let res2 (set res i v))
          (tail await_all kids res2 l2)))
        (Crashed (i m) (block (tail fail kids l2 m)))
        (Cancelled (i) (block (let u (str "unreachable: all! saw Cancelled")) (crash u)))))))

(proc both! (f g) eff
  (let fs (list f g))
  (let res (call all! fs))
  (let a (get res 0))
  (let b (get res 1))
  (ret (tup a b)))

(proc all_ok! (fs) eff
  (let n (len fs))
  (let kids (call spawn_all fs 0 n (list)))
  (let res (call zeros n (list)))
  (tail await_ok kids res n))

(proc await_ok (kids res left) eff
  (let z (eq left 0))
  (if z
    (block (ret (tag Ok res)))
    (block
      (let r (await kids))
      (let l2 (sub left 1))
      (match r
        (Done (i v) (block
          (match v
            (Ok (x) (block
              (let res2 (set res i x))
              (tail await_ok kids res2 l2)))
            (Err (e) (block
              (let n (len kids))
              (do (call cancel_all kids 0 n))
              (do (call drain kids l2))
              (ret (tag Err e)))))))
        (Crashed (i m) (block (tail fail kids l2 m)))
        (Cancelled (i) (block (let u (str "unreachable: all_ok! saw Cancelled")) (crash u)))))))

(proc race! (f fs) eff
  (let first (list f))
  (let n (len fs))
  (let kids0 (call spawn_all first 0 1 (list)))
  (let kids (call spawn_all fs 0 n kids0))
  (let r (await kids))
  (let left (len kids))
  (let l2 (sub left 1))
  (match r
    (Done (i v) (block
      (do (call cancel_all kids 0 left))
      (do (call drain kids l2))
      (ret v)))
    (Crashed (i m) (block (tail fail kids l2 m)))
    (Cancelled (i) (block (let u (str "unreachable: race! saw Cancelled")) (crash u)))))

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
