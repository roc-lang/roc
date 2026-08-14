# PR #9795 — Prevent hot-reload descriptor slots from overlapping image bytes

- **Author:** eluvane · **Draft:** no · **Base:** `main`
- **Size:** +60 / −23 in one file (`src/cli/main.zig`)

Two changes to `hotReloadChooseImageAllocation`: reclaimed free regions are
clipped to `tracker.descriptor_floor` (and skipped entirely when they start
above it), and the out-of-memory rejection is re-keyed from
`best_region == null` to `descriptor_slot.fresh`.

## Verdict

The problem is real and the clipping is the right idea. **But the fix has an
ordering hole that lets the exact overlap it targets still occur** (#1), and the
re-keyed OOM condition is a behavior regression the PR body presents as purely
additive (#2). I'd want #1 resolved before merge.

---

## Findings

### 1. (Likely bug) The clip uses a floor that `hotReloadChooseDescriptorSlot` then lowers

The order in `hotReloadChooseImageAllocation` is:

```zig
var best_region: ?HotReloadFreeRegion = null;
for (tracker.free_regions.items) |region| {
    if (region.start >= tracker.descriptor_floor) continue;
    const bounded_region = HotReloadFreeRegion{
        .start = region.start,
        .end = @min(region.end, tracker.descriptor_floor),   // <- clipped against the CURRENT floor
    };
    ...
}

const descriptor_slot = try hotReloadChooseDescriptorSlot(gpa, tracker);
```

But `hotReloadChooseDescriptorSlot` **mutates the floor** when it hands out a
fresh slot (`main.zig:4643`):

```zig
const offset = tracker.next_descriptor_offset;
...
tracker.descriptor_floor = offset;          // descriptors grow DOWN, so this lowers the floor
tracker.next_descriptor_offset = next_offset;
```

So when `free_descriptor_slots` is empty, the region was clipped against the
*old, higher* floor and the floor then drops by one descriptor slot. The
returned allocation reads the floor **after** the mutation:

```zig
return .{
    ...
    .image_limit = tracker.descriptor_floor,   // NEW (lower) floor
    .region_start = region.start,
    .region_end = region.end,                  // clipped to the OLD (higher) floor
```

That makes `region_end > image_limit` reachable, and the excess is exactly the
freshly-allocated descriptor slot.

Concrete scenario:

1. `descriptor_floor == desc0_offset`; `free_descriptor_slots` is empty;
   `next_descriptor_offset == desc1_offset` (and `desc1_offset < desc0_offset`).
2. A reclaimed image region `[X, desc0_offset)` exists. It starts below the
   floor, so it survives the `continue`, and `@min(region.end, desc0_offset)`
   leaves it unchanged.
3. `hotReloadChooseDescriptorSlot` takes a fresh slot at `desc1_offset` and sets
   `descriptor_floor = desc1_offset`.
4. The returned allocation has `region_end == desc0_offset` but
   `image_limit == desc1_offset`. The bytes `[desc1_offset, desc0_offset)` — the
   descriptor slot just handed to the rebuild worker — are inside the image
   region.

The `descriptor_slot.fresh and tracker.descriptor_floor <= append_offset` guard
doesn't catch this: that condition is about *append* storage reaching the
descriptor area, and here the collision comes from a reclaimed region, with
`append_offset` possibly far below.

Two ways to close it, either fine:

- **Move the region loop after `hotReloadChooseDescriptorSlot`**, so the clip
  and `image_limit` both see the post-allocation floor. This looks like a
  straight reorder; the loop reads only `tracker.free_regions` and
  `tracker.descriptor_floor`, neither of which the choose call invalidates
  apart from the floor itself.
- **Clip to `@min(tracker.descriptor_floor, tracker.next_descriptor_offset)`**,
  which is conservative regardless of whether the slot ends up fresh.

The first is cleaner and makes the invariant `region_end <= image_limit`
structurally obvious.

**Why the new test doesn't catch it:** `"hot reload allocation clips reclaimed
region to descriptor floor"` calls
`hotReloadReleaseDescriptorSlot(..., desc1_offset, true)` first, so
`free_descriptor_slots` is non-empty and `chooseDescriptorSlot` takes the
**pooled** path — which doesn't touch the floor. The test only exercises the
`fresh == false` case. A companion test that leaves the pool empty and asserts
`allocation.region_end <= allocation.image_limit` would fail today and pass
after the reorder. That assertion is worth adding regardless, since it states
the property the whole PR is about.

### 2. (Behavior regression, undisclosed) The OOM condition got strictly broader

```diff
-    if (best_region == null and tracker.descriptor_floor <= append_offset) {
+    if (descriptor_slot.fresh and tracker.descriptor_floor <= append_offset) {
```

The old condition rejected only when there was *nothing* to reuse. The new one
rejects whenever a fresh slot is needed and append storage has reached the
descriptor area — **even when a perfectly usable reclaimed region exists**.

The PR renames the test that documented the old behavior, which makes the change
unmistakable:

```diff
-test "hot reload allocation can use reclaimed region when append has no room"
+test "hot reload allocation rejects fresh descriptor when append has no room despite reclaimed region"
```

I think the new behavior is **correct** — a fresh slot consumes descriptor space
at `next_descriptor_offset`, and if `append_offset` has already grown past the
floor then taking another slot pushes the descriptor area into append storage no
matter where the image itself lands. So the image having somewhere to go is
irrelevant to whether the *descriptor* fits.

But it means hot reloads that succeed today will now fail with
`error.OutOfMemory`. The PR body describes this as "Fresh descriptor allocation
*also* fails when…", which reads as an added guard rather than a removed
capability. It should say plainly that a previously-supported case is now
rejected, and ideally note what the user sees when it happens (does the reload
fail gracefully and keep the old image live, or does the dev loop stop?).

### 3. (Coordination) This conflicts with the same author's #9805

Both PRs change `testingHotReloadDescriptor` / `testingPrepareHotReloadDescriptor`
to route through `hotReloadDescriptorForWrite` and thread `try` through the same
eight call sites — #9805 by deleting the wrapper and inlining, this one by
keeping the wrapper and changing its return type. They will conflict textually
and the resolutions differ.

Pick one home for it. This PR is the natural one (it's the hot-reload PR);
#9805's copy should come out, since it's unrelated scope there anyway.

Worth noting for whoever merges: the Windows `SEC_RESERVE` reasoning behind this
helper change is also the subject of PR #9809, so three PRs are touching the
same three lines.

## Things I verified and found correct

- **The clip arithmetic is sound.** The `region.start >= tracker.descriptor_floor`
  early-`continue` guarantees `start < floor`, so
  `end = @min(region.end, floor) > start` and `bounded_region.len()` can't
  underflow. ✓
- **Selection still picks the largest region**, now measured post-clip
  (`bounded_region.len() > best_region.?.len()`), which is the right comparison —
  comparing pre-clip lengths could pick a region that clips down to nothing
  useful. ✓
- **`hotReloadReturnDescriptorSlotAfterFailedChoice` correctly restores the floor**
  on the rejection path (`main.zig:4658-4662`): it raises
  `descriptor_floor` back to `slot.offset + descriptor_size` and rewinds
  `next_descriptor_offset`, so a rejected allocation doesn't permanently consume
  a slot. The tests assert `free_descriptor_slots.items.len == 0` afterward,
  which pins that the fresh slot was rewound rather than pushed onto the pool. ✓
- **The new test's expectations are self-consistent**: free region
  `[8192, mapped_size]`, floor at `desc1_offset`, so `region_start == 8192` and
  `region_end == desc1_offset` — the clip is doing exactly what it claims. And
  `preserve_descriptor_refs == true` correctly reflects the pooled slot that was
  released with `preserve = true`. ✓
- **The test helper change is a real improvement** independent of everything
  else: `@ptrCast(@alignCast(shm.base_ptr + offset))` bypassed the commit step
  that `hotReloadDescriptorForWrite` performs, so on Windows the tests were
  writing to reserved-but-uncommitted pages while production code committed
  first. Routing tests through the production path is the right call. ✓
