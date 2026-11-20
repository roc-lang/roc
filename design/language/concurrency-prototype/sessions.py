"""One-shot session channels (concurrency-sessions.md).

`pair()` makes a channel and returns its two ends, `(Out(a), In(a))`. One
message goes from the Out end to the In end:

    send(out, v)   never suspends
    recv(in)       a suspension site; returns Got(v), or Cancelled when the
                   Out end went away unused

Each end may be used once. A second use is a user error and crashes the task
(dynamic affinity; Padovani's FuSe checks linearity the same way). An end is
consumed by its use, so a well-typed protocol continues by sending a fresh
end along with its message.

When the last reference to an unused Out end goes away (dropped by code, by a
cancelled task's paused records, or by a finished task), the receiver gets
`Cancelled` instead of a message. When the In end goes away unused, a message
already sent, or sent later, is dropped.

Every end belongs to one task, its holder: the task that made the channel, or
the holder of the In end of the channel the end was last sent on, or the
parent an entry's result carrying it arrived at. A crashed task leaks what its
running frames held, as every crash does, so its ends are never dropped;
instead the crash closes every end the task held: an unused Out end as if
dropped (its receiver gets `Cancelled`), an unused In end as if gone (messages
to it are dropped). A send on a closed channel drops its message.

The runtime (Tasks builds) and the None engine share this state; they differ
only in how a waiting receiver is readied.
"""

from __future__ import annotations

from heap import Ref

USED_TWICE = "a session endpoint was used twice"
DEADLOCK = "deadlock: no task can ever send the message this task waits for"


class Chan:
    __slots__ = ("cid", "state", "value", "out_used", "in_used", "in_gone", "waiter", "out_holder", "in_holder")

    def __init__(self, cid):
        self.cid = cid
        self.state = "empty"  # empty full taken cancelled
        self.value = None
        self.out_used = self.in_used = self.in_gone = False
        self.waiter = None  # the task parked at this channel's receive
        self.out_holder = self.in_holder = None  # the tasks the ends belong to (Tasks builds)


class Sessions:
    def __init__(self, heap, stats, wake):
        self.heap, self.stats, self.wake = heap, stats, wake
        self.chans = {}
        heap.on_free = self.freed
        self.no_cancel_on_drop = False  # mutation switch: an unused Out end vanishes silently
        self.no_close_on_crash = False  # mutation switch: a crash closes none of its task's ends
        self.no_close_abandoned = False  # mutation switch: an abandoned receive leaves its channel open

    def pair(self, holder=None):
        c = Chan(len(self.chans) + 1)
        c.out_holder = c.in_holder = holder
        self.chans[c.cid] = c
        self.stats["channels"] += 1
        heap = self.heap
        o = heap.alloc("out", c.cid, [])
        i = heap.alloc("in", c.cid, [])
        return heap.alloc("tup", None, [o, i])

    def send(self, o, v):
        """Consumes both arguments. Returns a crash message or None."""
        heap = self.heap
        c = self.chans[heap.obj(o).tag]
        if c.out_used:
            heap.drop(v)
            heap.drop(o)
            return USED_TWICE
        c.out_used = True
        heap.drop(o)
        self.stats["messages"] += 1
        if c.in_gone or c.state == "cancelled":
            heap.drop(v)  # nobody will receive it, or a crash closed the channel
            return None
        c.state, c.value = "full", v
        self.transfer(v, c.in_holder)
        self.wake(c)
        return None

    def recv(self, i):
        """Consumes the end. ("ready", value), ("empty", chan) or ("crash", msg)."""
        heap = self.heap
        c = self.chans[heap.obj(i).tag]
        if c.in_used:
            heap.drop(i)
            return ("crash", USED_TWICE)
        c.in_used = True
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

    def transfer(self, v, holder):
        """The ends inside a value that crosses to another task now belong to
        it. A compiler generates this walk from the value's type; it visits
        nothing for types that cannot hold an end."""
        heap, stack, seen = self.heap, [v], set()
        while stack:
            r = stack.pop()
            if not isinstance(r, Ref) or r.id in seen:
                continue
            seen.add(r.id)
            o = heap.objs[r.id]
            if o.kind == "out":
                self.chans[o.tag].out_holder = holder
            elif o.kind == "in":
                self.chans[o.tag].in_holder = holder
            stack.extend(o.fields)

    def close_held(self, t):
        """A crash: close every end task t held."""
        if self.no_close_on_crash:
            return
        for c in self.chans.values():
            if c.out_holder is t and not c.out_used and c.state == "empty":
                c.state = "cancelled"
                self.stats["ends closed by a crash"] += 1
                self.wake(c)
            if c.in_holder is t and not c.in_used and not c.in_gone:
                c.in_gone = True
                self.stats["ends closed by a crash"] += 1
                if c.state == "full":
                    v, c.value = c.value, None
                    self.heap.drop(v)

    def abandon(self, c):
        """A task parked at a receive on c was cancelled or crashed. The
        receive had consumed the In end, so the end is gone: a message already
        sent is dropped, and one sent later is dropped at the send. Without
        this, a reply sent to a cancelled requester (a permit, a connection)
        would stay in the channel and never return to its owner."""
        if self.no_close_abandoned:
            return
        c.in_gone = True
        self.stats["receives abandoned"] += 1
        if c.state == "full":
            v, c.value = c.value, None
            self.heap.drop(v)

    def held(self):
        """Messages sent and not yet received, at the end of a run: a channel
        owns its message until the receive takes it or the In end goes away.
        Only a channel whose In end still exists (in a crashed frame, say) owns
        one; a message left behind a receive that was abandoned without
        closing its channel is owned by nothing, and the heap reports it."""
        heap = self.heap
        return [c.value for c in self.chans.values() if c.state == "full" and not c.in_used
                and not (isinstance(c.value, Ref) and heap.objs[c.value.id].freed)]  # a region host freed it

    def freed(self, o):
        c = self.chans[o.tag]
        if o.kind == "out" and not c.out_used:
            if self.no_cancel_on_drop:
                return
            c.state = "cancelled"
            self.wake(c)
        elif o.kind == "in" and not c.in_used:
            c.in_gone = True
            if c.state == "full":
                v, c.value = c.value, None
                self.heap.drop(v)
