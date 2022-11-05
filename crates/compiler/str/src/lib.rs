//! Provides `Roc` styled collection [reference counting](https://en.wikipedia.org/wiki/Reference_counting). 
//! This means the collection may or may not be safe to mutate in-place, may or 
//! may not be reference counted, and may or may not need to be freed when no 
//! longer in use. Whether each of these is true for a given collection can be 
//! determined by inspecting that collection at runtime.
#![crate_type = "lib"]
#![no_std]

/// Str does Roc-style collection reference counting, which means the collection
/// may or may not be safe to mutate in-place, may or may not be reference counted,
/// and may or may not need to be freed when no longer in use. Whether each of
/// these is true for a given collection can be determined by inspecting
/// that collection at runtime.
///
/// Details:
///
/// 1. If the collection is empty, it does not allocate on the heap.
/// 2. If it is nonempty, its pointer points to the first element of a "backing array" on the heap.
/// 3. There is an extra `isize` right before that backing array (still on the heap) which stores the
///    "flexible reference count" ("flexcount" for short).
/// 4. The flexcount can refer to one of three things, depending on whether it is positive,
///    negative, or zero.
/// 5. If the flexcount is positive, then it's a capacity. The capacity refers to the number of
///    collection elements in the backing array. This collection can be mutated in-place, until it
///    runs out of capacity. At that point, it will need a new backing array. Once it goes out of
///    scope, the backing array should be freed by the system allocator - but free() must be passed
///    a pointer to the flexcount slot, not to element 0 (because the flexcount slot is where the
///    original allocation began). Capacity will always be at least 1, because otherwise we would
///    not have allocated on the heap in the first place.
/// 6. If the flexcount is 0, then this collection resides in readonly memory. That means it cannot
///    be mutated in-place (and attempting to do so will segfault), and it must not be attempted to
///    be freed. It exists in memory forever!
/// 7. If the flexcount is negative, then it is a reference count. Treat the collection as immutable, just like
///    if the flexcount were 0, except free it when there are no more references to it. Instead of the reference count
///    starting at 0 or 1 and incrementing when new references are added, this refcount starts with all bits being 1 (so, isize::MIN) and
///    increments towards 0 when new references are added. When a reference is removed, if all bits are 1, then it should be freed. If so many new references are added that it gets incremented all the way from isize::MAX to 0, then, as is best practice when running out of reference counts, it will leak. (Leaking memory is typically less bad than crashing, and this should essentially never happen.) This happens automatically because when the flexcount is 0, it's assumed that the collection is in readonly memory and should not be freed - which is nice because it means there is no extra conditional required to implement this edge case.
/// 8. If a collection has a refcount of isize::MIN (meaning nothing else references it), it may or may not be safe to convert it to a capacity,
///    depending on whether it contains other refcounted collections. For example, a Str
///    is a collection of all bytes, so if it has a refcount of all 1 bits, it can be safely
///    converted to a capacity (the initial capacity should be equal to the collection's length),
///    after which point it can be safely mutated in-place. However, a refcounted List of Lists with a refcount of isize::MIN will not be safe to convert to a capacity, unless the inner Lists also happen to have refcounts of isize::MIN. This is because mutate-in-place operations like removing an element from a list do not check for refcounts in the elements they remove, which means removing an element from the newly mutable-in-place list would cause memory leaks in its refcounted contents. (They'd have been removed, but their reference counts would not have been adjusted accordingly.)
///
/// Note that because of these runtime conditionals, modifying and freeing Roc collections are both
/// cheaper operations in generated Roc code than in host code. Since the Roc compiler knows
/// statically whether a collection is refcounted, unique, or readonly, it does not bother with
/// these checks at runtime. A host, however, cannot have that information statically (since it may be different
/// for different applications), and so must check at runtime instead.
struct Str {
    bytes: [16, u8];
}

#[no_mangle]
pub fn empty_() -> Str {
    Str {
        bytes : [0; 16]
    }
}

#[no_mangle]
pub fn len_(string: Str) -> usize {
    let disc = discriminant(str);

    if disc == 0 {
        // It's a
    }
}

#[inline(always)]
fn discriminant(string: &Str) -> u8 {
    // cast the first 8 bytes to be u64, return its lsbyte
}
