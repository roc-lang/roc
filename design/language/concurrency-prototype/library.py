"""Library patterns over channels and mailboxes (concurrency-sessions.md,
"Shared resources are libraries" and "Library patterns"), written in the IR as
a library author would write them in Roc.

Semaphore (as in the RFC's guide)
    sem_serve(inbox, permits)   the semaphore: a task that owns the count,
                                serves requests from a mailbox and hands out
                                permits, each the Out end of a channel back
                                to itself; returns the free count once every
                                Post has gone and every permit has come back
    with_permit(requests, f)    run f while holding one permit
    with_permit_remote(requests, k)
                                pass the permit to a hosted operation, which
                                holds it until the platform has finished
    sem_with(permits, body)     run body(requests) with a semaphore beside it;
                                returns (body's result, the free count)

Pool
    pool_serve(inbox, size)     a semaphore whose grant carries a resource and
                                whose Release returns it; a lease that ends in
                                Cancelled lost its resource, so the pool opens
                                a replacement. Returns (free, opened)
    with_resource(requests, f)  run f(resource) while holding a lease

Semaphore cell (the experiment of "Decisions left to prototypes" (b))
    with_cell_permit(s, f)      acquire a permit from cell s, run f, release it

A permit is released when it is dropped, and ARC drops a value after its last
use, not at the end of a scope. So each of these uses its permit after `f`
returns (`send permit Release`, `call release permit`). A version that does
not, `with_cell_permit_early`, releases the permit before `f` runs; the
`Libraries` tests keep it to show this.
"""

from ir import parse

SOURCE = r"""
; ------------------------------------------------------------- lists
(proc list_rest (l i n acc) pure
  (let c (lt i n))
  (if c
    (block
      (let x (get l i))
      (let acc2 (push acc x))
      (tail list_rest l (add i 1) n acc2))
    (block (ret acc))))

; ------------------------------------------------------------- semaphore
(proc sem_ask (b) eff
  (let r (take b))
  (ret (tag Request r)))

; Hand out one permit and wait until it comes back, however it comes back.
(proc sem_grant (reply) eff
  (let pp (pair))
  (let permit (field pp 0))
  (let back (field pp 1))
  (do (send reply permit))
  (let r (recv back))
  (ret (tag Returned)))

(proc sem_serve (inbox permits) eff
  (let g (group (call concurrently (list (clo sem_ask inbox)))))
  (tail sem_loop g permits (list)))

(proc sem_loop (g free queue) eff
  (let a (next g))
  (match a
    (Arrived (ev) (block
      (match ev
        (Request (r) (block
          (match r
            (Got (reply rest) (block
              (do (spawn_in g (clo sem_ask rest)))
              (let some (lt 0 free))
              (if some
                (block
                  (do (spawn_in g (clo sem_grant reply)))
                  (tail sem_loop g (sub free 1) queue))
                (block (tail sem_loop g free (push queue reply))))))
            (Closed () (block (tail sem_loop g free queue))))))
        (Returned () (block
          (let n (len queue))
          (let none (eq n 0))
          (if none
            (block (tail sem_loop g (add free 1) queue))
            (block
              (let reply (get queue 0))
              (let rest (call list_rest queue 1 n (list)))
              (do (spawn_in g (clo sem_grant reply)))
              (tail sem_loop g free rest))))))))
    (Cancelled () (block (crash "unreachable: a loop body saw Cancelled")))
    (Ended () (block (ret free)))))

(proc with_permit (requests f) eff
  (let rp (pair))
  (let reply (field rp 0))
  (let granted (field rp 1))
  (do (post requests reply))
  (let r (recv granted))
  (match r
    (Got (permit) (block
      (let v (callc! f))
      (do (send permit (tag Release)))
      (ret v)))
    (Cancelled () (block (crash "the semaphore stopped")))))

(proc with_permit_remote (requests k) eff
  (let rp (pair))
  (let reply (field rp 0))
  (let granted (field rp 1))
  (do (post requests reply))
  (let r (recv granted))
  (match r
    (Got (permit) (block
      (let v (host remote permit k))
      (ret v)))
    (Cancelled () (block (crash "the semaphore stopped")))))

(proc sem_body (body requests) eff
  (let v (callc! body requests))
  (ret v))

(proc sem_with (permits body) eff
  (let mb (mailbox))
  (let requests (field mb 0))
  (let inbox (field mb 1))
  (let r (call both! (clo sem_body body requests) (clo sem_serve inbox permits)))
  (ret r))

; ------------------------------------------------------------- pool
; A lease is (resource, Out([Release(resource)])). Resources are numbers
; here; opening one is a hosted call.
(proc pool_grant (reply res) eff
  (let pp (pair))
  (let lease_end (field pp 0))
  (let back (field pp 1))
  (do (send reply (tup res lease_end)))
  (let r (recv back))
  (match r
    (Got (m) (block (match m (Release (x) (block (ret (tag Back x)))))))
    (Cancelled () (block (ret (tag Lost))))))

(proc pool_serve (inbox size) eff
  (let g (group (call concurrently (list (clo sem_ask inbox)))))
  (tail pool_loop g (call pool_init size (list)) (list) 0))

(proc pool_init (n acc) pure
  (let z (eq n 0))
  (if z (block (ret acc)) (block (tail pool_init (sub n 1) (push acc n)))))

(proc pool_loop (g free queue opened) eff
  (let a (next g))
  (match a
    (Arrived (ev) (block
      (match ev
        (Request (r) (block
          (match r
            (Got (reply rest) (block
              (do (spawn_in g (clo sem_ask rest)))
              (let nf (len free))
              (let some (lt 0 nf))
              (if some
                (block
                  (let res (get free 0))
                  (let free2 (call list_rest free 1 nf (list)))
                  (do (spawn_in g (clo pool_grant reply res)))
                  (tail pool_loop g free2 queue opened))
                (block (tail pool_loop g free (push queue reply) opened)))))
            (Closed () (block (tail pool_loop g free queue opened))))))
        (Back (x) (block (tail pool_back g free queue opened x)))
        (Lost () (block
          (let fresh (host slow opened))
          (tail pool_back g free queue (add opened 1) fresh))))))
    (Cancelled () (block (crash "unreachable: a loop body saw Cancelled")))
    (Ended () (block (ret (tup (len free) opened))))))

; A resource came back (or its replacement was opened): lend it to the
; first waiter, or keep it.
(proc pool_back (g free queue opened res) eff
  (let n (len queue))
  (let none (eq n 0))
  (if none
    (block (tail pool_loop g (push free res) queue opened))
    (block
      (let reply (get queue 0))
      (let rest (call list_rest queue 1 n (list)))
      (do (spawn_in g (clo pool_grant reply res)))
      (tail pool_loop g free rest opened))))

(proc with_resource (requests f) eff
  (let rp (pair))
  (let reply (field rp 0))
  (let granted (field rp 1))
  (do (post requests reply))
  (let r (recv granted))
  (match r
    (Got (lease) (block
      (let res (field lease 0))
      (let lease_end (field lease 1))
      (let v (callc! f res))
      (do (send lease_end (tag Release res)))
      (ret v)))
    (Cancelled () (block (crash "the pool stopped")))))

(proc pool_body (body requests) eff
  (let v (callc! body requests))
  (ret v))

(proc pool_with (size body) eff
  (let mb (mailbox))
  (let requests (field mb 0))
  (let inbox (field mb 1))
  (let r (call both! (clo pool_body body requests) (clo pool_serve inbox size)))
  (ret r))

; ------------------------------------------------------------- semaphore cell
(proc release (permit) eff (ret 0))

(proc with_cell_permit (s f) eff
  (let permit (acquire s))
  (let v (callc! f))
  (do (call release permit))
  (ret v))

; Wrong on purpose: the permit's last use is the acquire, so ARC releases it
; at once and f runs without it.
(proc with_cell_permit_early (s f) eff
  (let permit (acquire s))
  (let v (callc! f))
  (ret v))
"""

# ------------------------------------------------------------- shared state
# The same libraries over Shared handles (concurrency-shared-variables.md).
# Each `step` is an `update!`; its pure procedure returns Set(result, new
# state), Keep(result) or Wait. A `defer` ... `undefer` pair is a `finally!`
# around the code between them, and an `errdefer` ... `undefer` pair is an
# `on_failure!`.
SHARED_SOURCE = r"""
; ---- semaphore
;   with_permit! = |permits, f| {
;       permits.update!(|n| if n == 0 { Wait } else { Set(n - 1, {}) })
;       permits.finally!(|n| n + 1, f)
;   }
(proc sv_take (n) pure
  (let z (eq n 0))
  (if z (block (ret (tag Wait))) (block (ret (tag Set 0 (sub n 1))))))

(proc sv_give (n) pure (ret (tag Set 0 (add n 1))))

(proc sv_with_permit (s f) eff
  (do (step sv_take (s)))
  (do (defer sv_give (s)))
  (let v (callc! f))
  (do (undefer))
  (ret v))

; ---- pool
; The cell holds (free resources, resources lost). A lease that ends by
; cancellation or a crash loses its resource (on_failure!); the next borrower
; that finds none free but some lost opens a replacement, outside the update.
(proc pool_take_sv (st) pure
  (let free (field st 0))
  (let lost (field st 1))
  (let n (len free))
  (let some (lt 0 n))
  (if some
    (block
      (let res (get free 0))
      (let rest (call list_rest free 1 n (list)))
      (ret (tag Set (tag Lend res) (tup rest lost))))
    (block
      (let any_lost (lt 0 lost))
      (if any_lost
        (block (ret (tag Set (tag Open) (tup free (sub lost 1)))))
        (block (ret (tag Wait)))))))

(proc pool_give_sv (st res) pure
  (ret (tag Set 0 (tup (push (field st 0) res) (field st 1)))))

(proc pool_lose_sv (st) pure
  (ret (tag Set 0 (tup (field st 0) (add (field st 1) 1)))))

(proc lease_res (d) eff
  (match d
    (Lend (res) (block (ret res)))
    (Open () (block (let fresh (host slow 50)) (ret fresh)))))

; The on_failure! begins right after the take, before a replacement is
; opened, so a borrower cancelled while opening one gives the slot back.
(proc sv_with_resource (p f) eff
  (let d (step pool_take_sv (p)))
  (do (errdefer pool_lose_sv (p)))
  (let res (call lease_res d))
  (let v (callc! f res))
  (do (undefer))
  (do (step pool_give_sv (p) res))
  (ret v))

; The pool with finally! instead: it takes back a resource whose user
; was cancelled or crashed, in whatever state that left it.
(proc sv_with_resource_plain (p f) eff
  (let d (step pool_take_sv (p)))
  (let res (call lease_res d))
  (do (defer pool_give_sv (p) res))
  (let v (callc! f res))
  (do (undefer))
  (ret v))

; ---- circuit breaker
; Closed(failures), Open(until), Probing. `now` is passed in: an update
; cannot read the clock.
(proc br_decide (st now) pure
  (match st
    (Closed (n) (block (ret (tag Keep (tag Run)))))
    (Probing () (block (ret (tag Keep (tag Reject)))))
    (Open (until) (block
      (let early (lt now until))
      (if early
        (block (ret (tag Keep (tag Reject))))
        (block (ret (tag Set (tag Probe) (tag Probing)))))))))

(proc br_record (st ok now) pure
  (if ok
    (block (ret (tag Set 0 (tag Closed 0))))
    (block
      (match st
        (Closed (n) (block
          (let more (lt n 2))
          (if more
            (block (ret (tag Set 0 (tag Closed (add n 1)))))
            (block (ret (tag Set 0 (tag Open (add now 5))))))))
        (Probing () (block (ret (tag Set 0 (tag Open (add now 5))))))
        (Open (u) (block (ret (tag Keep 0))))))))

; The finally! cleanup of a probe: a probe that ended without recording
; reopens.
(proc br_reopen (st now) pure
  (match st
    (Probing () (block (ret (tag Set 0 (tag Open (add now 5))))))
    (Closed (n) (block (ret (tag Keep 0))))
    (Open (u) (block (ret (tag Keep 0))))))

(proc br_ok (r) pure
  (match r (Ok (x) (block (ret 1))) (Err (e) (block (ret 0)))))

(proc br_run (b now f) eff
  (let r (callc! f))
  (let ok (call br_ok r))
  (do (step br_record (b) ok now))
  (ret r))

(proc br_guarded (b now f) eff
  (let d (step br_decide (b) now))
  (match d
    (Reject () (block (ret (tag Err "open"))))
    (Run () (block (tail br_run b now f)))
    (Probe () (block
      (do (defer br_reopen (b) now))
      (let r (call br_run b now f))
      (do (undefer))
      (ret r)))))
"""

LIBRARY = parse(SOURCE)
