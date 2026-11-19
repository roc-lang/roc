"""Reference-counted heap with exact accounting.

Values are Python ints (unboxed), `Ref` (a counted heap object) or `Handle`
(a child task; Roc owns child tasks, so handles are not counted). Freed
objects stay in the table, so any later access is caught as a use after free.
"""

from __future__ import annotations

from collections import Counter


class HeapError(Exception):
    pass


class Ref:
    __slots__ = ("id",)

    def __init__(self, i):
        self.id = i

    def __repr__(self):
        return f"#{self.id}"


class Handle:
    __slots__ = ("task",)

    def __init__(self, t):
        self.task = t

    def __repr__(self):
        return f"task{self.task}"


class Obj:
    __slots__ = ("kind", "tag", "fields", "rc", "freed", "owner", "thread")

    def __init__(self, kind, tag, fields, owner, thread):
        self.kind, self.tag, self.fields, self.rc, self.freed = kind, tag, fields, 1, False
        self.owner, self.thread = owner, thread


class Heap:
    """Exact reference-counted heap. Each object remembers the task and the
    thread that allocated it: `confined` models an allocator that can release
    an allocation only on its own thread (P6), and `bulk_free` a host that
    reclaims a crashed tree's memory as a region (P9)."""

    def __init__(self):
        self.objs = {}
        self.next = 0
        self.stats = Counter()
        self.task = None  # the task running now (None: the host)
        self.thread = 0  # the thread running now
        self.confined = False

    def alloc(self, kind, tag, fields):
        self.next += 1
        self.objs[self.next] = Obj(kind, tag, list(fields), self.task, self.thread)
        self.stats["alloc"] += 1
        return Ref(self.next)

    def obj(self, v):
        if not isinstance(v, Ref):
            raise HeapError(f"expected a heap value, got {v!r}")
        o = self.objs[v.id]
        if o.freed:
            raise HeapError(f"use after free of #{v.id}")
        return o

    def dup(self, v):
        if isinstance(v, Ref):
            self.obj(v).rc += 1
            self.stats["dup"] += 1

    def drop(self, v):
        if not isinstance(v, Ref):
            return
        stack = [v]
        while stack:
            r = stack.pop()
            o = self.obj(r)
            self.stats["drop"] += 1
            o.rc -= 1
            if o.rc == 0:
                if self.confined and o.thread != self.thread:
                    raise HeapError(f"P6: #{r.id}, allocated on thread {o.thread}, released on thread {self.thread}")
                o.freed = True
                self.stats["free"] += 1
                stack.extend(f for f in o.fields if isinstance(f, Ref))

    def bulk_free(self, ids):
        """Free objects as a region: no counts are read or changed."""
        for i in ids:
            self.objs[i].freed = True
            self.stats["region freed"] += 1

    def unique(self, v):
        return self.obj(v).rc == 1

    def data(self, v):
        """A structural copy, for comparing results across runs."""
        if isinstance(v, int):
            return v
        if isinstance(v, Handle):
            return ("handle",)
        o = self.obj(v)
        if o.kind == "str":
            return ("str", o.tag)
        return (o.kind, o.tag, tuple(self.data(f) for f in o.fields))

    def live(self):
        return {i for i, o in self.objs.items() if not o.freed}

    def verify(self, roots):
        """Every live object must be accounted for exactly: its count equals the
        references to it from live objects plus those in `roots` (leaked
        native-frame values). Returns the number of live objects."""
        want = Counter()
        for r in roots:
            if isinstance(r, Ref):
                want[r.id] += 1
        live = self.live()
        for i in live:
            for f in self.objs[i].fields:
                if isinstance(f, Ref):
                    want[f.id] += 1
        for i in live:
            if self.objs[i].rc != want[i]:
                raise HeapError(f"#{i} has count {self.objs[i].rc} but {want[i]} references")
        for i in want:
            if i not in live:
                raise HeapError(f"#{i} is referenced but was freed")
        reach, stack = set(), [r.id for r in roots if isinstance(r, Ref)]
        while stack:
            i = stack.pop()
            if i in reach:
                continue
            reach.add(i)
            stack.extend(f.id for f in self.objs[i].fields if isinstance(f, Ref))
        if live - reach:
            raise HeapError(f"leaked objects not held by any crashed frame: {sorted(live - reach)[:10]}")
        return len(live)
