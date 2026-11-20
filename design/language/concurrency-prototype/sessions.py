"""Session channels, mailboxes and semaphore cells (concurrency-sessions.md).

One-shot channels. `pair()` makes a channel and returns its two ends,
`(Out(a), In(a))`. One message goes from the Out end to the In end:

    send(out, v)   never suspends
    recv(in)       a suspension site; returns Got(v), or Cancelled when the
                   Out end went away unused

Mailboxes. `mailbox()` returns `(Post(a), Inbox(a))`. A Post is an ordinary
shared value; posting never suspends. The Inbox has one reader:

    post(p, v)     append v; drops it if the inbox has gone
    take(b)        a suspension site; returns Got(v, b'), with the inbox to
                   take from next, or Closed once every Post has gone and the
                   queue is empty

Semaphore cells, the experiment of "Decisions left to prototypes" (b).
`sem(n)` returns a shared handle with n permits:

    acquire(s)     a suspension site; returns a Permit, waiting first-come
                   first-served until one is free
    dropping a Permit, by code, by a cancelled task's records or by closing,
    returns it to the cell. No Roc code runs.

Each Out end, In end, Inbox and Permit may be used once. A second use is a
user error and crashes the task (dynamic affinity; Padovani's FuSe checks
linearity the same way).

**Ports and their release.** Out ends, In ends, Posts, Inboxes and Permits
are ports: the runtime acts when one is released.

    Out end released unused     its receiver gets Cancelled
    In end released unused      its message, now or later, is dropped
    Post released (the last)    the inbox reports Closed once it is empty
    Inbox released unused       its queue, now or later, is dropped
    Permit released             it returns to its cell

A port is released when its last reference is dropped, or when its task
crashed with it in a running frame and no live task can still use it. A
crash leaks what its running frames held, so the second case needs holders.

**Holders.** Every port has a set of holder tasks: the tasks that may hold a
reference to it. It is exact enough to say when no live task can use the
port, without inspecting frames:

- A port starts held by the task that made it.
- When a value crosses to another task (an entry's captures when it starts,
  a message when it is sent or posted, a child's result when it arrives, a
  hosted call's arguments when the host takes them), every port reachable
  from it gains the receiving task. If the value reaches the port only
  through objects with a count of one, the value was the only path to it,
  and the receiving task becomes its only holder.
- A message waiting in a channel or an inbox travels with the In end or the
  Inbox: the walk continues into it.
- A task leaves every holder set when it finishes. When a live port's set
  becomes empty, the references left are in crashed frames, and the port is
  released.

The runtime (Tasks builds) and the None engine share this state; the None
engine tracks no holders (a crash ends a None program), and differs only in
how a waiting task is readied.
"""

from __future__ import annotations

from heap import Ref

USED_TWICE = "a session endpoint was used twice"
DEADLOCK = "deadlock: no task can ever send the message this task waits for"
HOST = "host"  # the holder of values the host keeps: hosted arguments, a root's result


class Chan:
    __slots__ = ("cid", "state", "value", "out_used", "in_used", "in_gone", "waiter", "out_port", "in_port")

    def __init__(self, cid):
        self.cid = cid
        self.state = "empty"  # empty full taken cancelled
        self.value = None
        self.out_used = self.in_used = self.in_gone = False
        self.waiter = None  # the task parked at this channel's receive
        self.out_port = self.in_port = None


class Box:
    __slots__ = ("bid", "queue", "gone", "closed", "waiter", "post_port", "inbox_port")

    def __init__(self, bid):
        self.bid = bid
        self.queue = []
        self.gone = False  # the reader's Inbox went away: posts are dropped
        self.closed = False  # every Post went away
        self.waiter = None
        self.post_port = self.inbox_port = None


class Sem:
    __slots__ = ("sid", "n", "free", "waiters", "granted")

    def __init__(self, sid, n):
        self.sid, self.n, self.free = sid, n, n
        self.waiters = []  # tasks parked at acquire, first come first served
        self.granted = []  # tasks a released permit was handed to, not yet resumed


class Port:
    __slots__ = ("kind", "target", "holders", "used", "done")

    def __init__(self, kind, target, holders):
        self.kind, self.target, self.holders = kind, target, holders
        self.used = False  # Out, In, Inbox: consumed by its one use
        self.done = False  # released already (closing, then a later free, acts once)


class Sessions:
    def __init__(self, heap, stats, wake, track=True):
        self.heap, self.stats, self.wake = heap, stats, wake
        self.track = track  # holders are tracked (Tasks builds)
        self.chans, self.boxes, self.sems = {}, {}, {}
        self.ports = {}  # heap object id -> Port
        heap.on_free = self.freed
        # Mutation switches for the regression tests.
        self.no_cancel_on_drop = False  # an unused Out end vanishes silently
        self.no_close_on_leave = False  # a port whose holders all left is never released
        self.no_close_abandoned = False  # an abandoned receive leaves its channel or inbox open
        self.no_capture_holders = False  # an entry's captures do not make it a holder
        self.no_release_cells = False  # a cell that no live task holds keeps its state
        self.released = []  # (cell object id, the holder whose leaving released it)

    def register(self, kind, r, target, holder):
        """Track holders for heap object r (a shared variable's cell): a
        port whose release drops the cell's state."""
        p = Port(kind, target, {holder} if self.track and holder is not None else set())
        self.ports[r.id] = p
        return p

    def new_port(self, kind, tag, target, holder):
        r = self.heap.alloc(kind, tag, [])
        p = Port(kind, target, {holder} if self.track and holder is not None else set())
        self.ports[r.id] = p
        return r, p

    # ------------------------------------------------------------ channels
    def pair(self, holder=None):
        c = Chan(len(self.chans) + 1)
        self.chans[c.cid] = c
        self.stats["channels"] += 1
        o, c.out_port = self.new_port("out", c.cid, c, holder)
        i, c.in_port = self.new_port("in", c.cid, c, holder)
        return self.heap.alloc("tup", None, [o, i])

    def send(self, o, v):
        """Consumes both arguments. Returns a crash message or None."""
        heap = self.heap
        c = self.chans[heap.obj(o).tag]
        if c.out_used:
            heap.drop(v)
            heap.drop(o)
            return USED_TWICE
        c.out_used = c.out_port.used = True
        heap.drop(o)
        self.stats["messages"] += 1
        if c.in_gone or c.state == "cancelled":
            heap.drop(v)  # nobody will receive it, or the channel was released
            return None
        c.state, c.value = "full", v
        self.transfer(v, c.in_port.holders)
        self.wake(c)
        return None

    def recv(self, i):
        """Consumes the end. ("ready", value), ("empty", chan) or ("crash", msg)."""
        heap = self.heap
        c = self.chans[heap.obj(i).tag]
        if c.in_used:
            heap.drop(i)
            return ("crash", USED_TWICE)
        c.in_used = c.in_port.used = True
        heap.drop(i)
        if c.state != "empty":
            return ("ready", self.take(c))
        return ("empty", c)

    def take(self, c):
        if c.state == "full":
            v, c.value, c.state = c.value, None, "taken"
            return self.heap.alloc("tag", "Got", [v])
        self.stats["Cancelled delivered"] += 1
        return self.heap.alloc("tag", "Cancelled", [])

    # ------------------------------------------------------------ mailboxes
    def mailbox(self, holder=None):
        b = Box(len(self.boxes) + 1)
        self.boxes[b.bid] = b
        self.stats["mailboxes"] += 1
        p, b.post_port = self.new_port("post", b.bid, b, holder)
        i, b.inbox_port = self.new_port("inbox", b.bid, b, holder)
        return self.heap.alloc("tup", None, [p, i])

    def post(self, p, v):
        """Consumes both arguments; never suspends."""
        heap = self.heap
        b = self.boxes[heap.obj(p).tag]
        self.stats["posts"] += 1
        if b.gone:
            heap.drop(v)
        else:
            b.queue.append(v)
            self.transfer(v, b.inbox_port.holders)
            self.wake(b)
        heap.drop(p)  # after the message is queued: the last Post closes the box behind it

    def take_box(self, i, holder=None):
        """Consumes the inbox. ("ready", value), ("empty", box) or ("crash", msg)."""
        heap = self.heap
        b = self.boxes[heap.obj(i).tag]
        port = self.ports[i.id]
        if port.used:
            heap.drop(i)
            return ("crash", USED_TWICE)
        port.used = True
        heap.drop(i)
        if b.queue or b.closed:
            return ("ready", self.deliver(b, holder))
        return ("empty", b)

    def deliver(self, b, holder=None):
        """Got(v, the next Inbox) or Closed. The next Inbox belongs to the reader."""
        if b.queue:
            v = b.queue.pop(0)
            nxt, b.inbox_port = self.new_port("inbox", b.bid, b, holder)
            return self.heap.alloc("tag", "Got", [v, nxt])
        self.stats["Closed delivered"] += 1
        return self.heap.alloc("tag", "Closed", [])

    # ------------------------------------------------------------ semaphore cells
    def sem(self, n):
        s = Sem(len(self.sems) + 1, n)
        self.sems[s.sid] = s
        self.stats["semaphore cells"] += 1
        return self.heap.alloc("sem", s.sid, [])

    def acquire(self, s, holder=None):
        """Consumes the handle. ("ready", permit) or ("empty", sem)."""
        heap = self.heap
        sem = self.sems[heap.obj(s).tag]
        heap.drop(s)
        if sem.free > 0:
            sem.free -= 1
            return ("ready", self.permit(sem, holder))
        return ("empty", sem)

    def permit(self, sem, holder=None):
        self.stats["permits"] += 1
        return self.new_port("permit", sem.sid, sem, holder)[0]

    def release(self, sem):
        """A permit came back: hand it to the first waiter, or free it."""
        self.stats["permits returned"] += 1
        if sem.waiters:
            w = sem.waiters.pop(0)
            sem.granted.append(w)
            self.wake(sem, w)
        else:
            sem.free += 1

    # ------------------------------------------------------------ holders
    def transfer(self, v, holders):
        """The ports inside a value that crosses to other tasks: their holders
        gain `holders`, which replace them when the value is the only path.
        A compiler generates this walk from the value's type; it visits nothing
        for types that cannot hold a port."""
        if not self.track:
            return
        holders = set(holders)  # it may be the set of a port this walk reaches
        heap, stack, seen = self.heap, [(v, True)], set()
        while stack:
            r, only = stack.pop()
            if not isinstance(r, Ref) or r.id in seen:
                continue
            seen.add(r.id)
            o = heap.objs[r.id]
            only = only and o.rc == 1
            p = self.ports.get(r.id)
            if p is not None and not p.done:
                if only:
                    p.holders.clear()
                p.holders |= holders
                if p.kind == "in" and p.target.state == "full" and not p.target.in_gone:
                    stack.append((p.target.value, only))
                elif p.kind == "inbox" and p.target.inbox_port is p and not p.target.gone:
                    stack.extend((m, only) for m in p.target.queue)
            stack.extend((f, only) for f in o.fields)

    def capture(self, clo, task):
        """An entry starts: the ports its closure captured are now (also) its."""
        if not self.no_capture_holders:
            self.transfer(clo, {task})

    def leave(self, t, kinds=None):
        """Task t finished (or the host, at the end of a run, for `kinds`). A
        live port that no live task holds any more is released. A channel end,
        post, inbox or permit is then held only by crashed frames. A cell may
        also be held by the states of other cells that no live task holds: a
        cycle, which releasing the cell's state breaks."""
        for rid, p in self.ports.items():
            if t not in p.holders or (kinds is not None and p.kind not in kinds):
                continue
            p.holders.discard(t)
            if p.holders or p.done or self.heap.objs[rid].freed:
                continue
            if self.no_release_cells if p.kind == "shared" else self.no_close_on_leave:
                continue
            if self.release_port(p):
                if p.kind == "shared":
                    self.stats["cells released"] += 1
                    self.stats["cells released at the end of the run" if t == HOST else
                               "cells released when a child task finished" if t.parent is not None else
                               "cells released when the root finished"] += 1
                    self.released.append((rid, t))
                else:
                    self.stats["ports released after a crash"] += 1

    def release_cells(self):
        """The end of a None build, whose one stack holds every cell it can
        still use until the program ends: release every cell still live."""
        if self.no_release_cells:
            return
        for rid, p in list(self.ports.items()):
            if p.kind == "shared" and not p.done and not self.heap.objs[rid].freed and self.release_port(p):
                self.stats["cells released"] += 1
                self.released.append((rid, None))

    def abandon(self, target):
        """A task parked at a receive (channel or inbox) was cancelled or
        crashed. The receive had consumed the end, so no code can ever take a
        message there: one already sent is dropped, and one sent later is
        dropped at the send. Without this, a reply sent to a cancelled
        requester (a permit, a connection) would stay there and never return
        to its owner."""
        if self.no_close_abandoned:
            return
        self.stats["receives abandoned"] += 1
        if isinstance(target, Chan):
            target.in_gone = True
            if target.state == "full":
                v, target.value = target.value, None
                self.heap.drop(v)
        else:
            self.drop_queue(target)

    def drop_queue(self, b):
        b.gone = True
        q, b.queue = b.queue, []
        for v in q:
            self.heap.drop(v)

    def release_port(self, p):
        """Act on a port's release, once. Returns whether it did anything."""
        if p.done:
            return False
        p.done = True
        k, x = p.kind, p.target
        if k == "out":
            if x.out_used or x.state != "empty" or self.no_cancel_on_drop:
                return False
            x.state = "cancelled"
            self.wake(x)
        elif k == "in":
            if x.in_used or x.in_gone:
                return False
            x.in_gone = True
            if x.state == "full":
                v, x.value = x.value, None
                self.heap.drop(v)
        elif k == "post":
            x.closed = True
            if not x.queue:
                self.wake(x)
        elif k == "inbox":
            if p.used or x.inbox_port is not p or x.gone:
                return False
            self.drop_queue(x)
        elif k == "permit":
            self.release(x)
        elif k == "shared":
            # Freed already (its state went with it), or no live task holds
            # it: drop its state, which breaks any cycle through it.
            o = self.heap.objs[x.rid]
            if o.freed:
                return False
            x.released = True
            v, o.fields[0] = o.fields[0], 0
            self.heap.drop(v)
        return True

    def freed(self, rid, o):
        self.release_port(self.ports[rid])

    def unreturned(self):
        """Semaphore cells missing permits once every task has finished: each
        permit comes back however its holder ended."""
        return [(s.sid, s.free, s.n) for s in self.sems.values() if s.free != s.n or s.waiters or s.granted]

    def held(self):
        """Messages sent and not yet received, at the end of a run: a channel
        owns its message until a receive takes it or the In end goes away, and
        an inbox owns its queue until its reader goes away. Only one whose
        reader end still exists (in a crashed frame, say) owns one; a message
        left behind a receive that was abandoned without closing is owned by
        nothing, and the heap reports it."""
        heap = self.heap
        live = lambda v: not (isinstance(v, Ref) and heap.objs[v.id].freed)  # a region host freed it
        out = [c.value for c in self.chans.values() if c.state == "full" and not c.in_used and live(c.value)]
        for b in self.boxes.values():
            if not b.inbox_port.used:
                out += [v for v in b.queue if live(v)]
        return out
