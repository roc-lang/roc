# `Str`

This is the in-memory representation for `Str`. To explain how `Str` is laid out in memory, it's helpful to start with how `List` is laid out.

## Empty list

An empty `List Str` is essentially this Rust type with all 0s in memory:

```rust
struct List {
    pointer: *Str, // pointers are the same size as `usize`
    length: usize
}
```

On a 64-bit system, this `struct` would take up 16B in memory. On a 32-bit sysem, it would take up 8B.

Here's what the fields mean:

* `pointer` is the memory address of the heap-allocated memory containing the `Bool` elements. For an empty list, the pointer is null (that is, 0).
* `length` is the number of `Bool` elements in the list. For an empty list, this is also 0.

## Nonempty list

Now let's say we define a `List Str` with two elements in it, like so: `[ "foo", "bar" ]`.

First we'd have the `struct` above, with both `length` and `capacity` set to 2. Then, we'd have some memory allocated on the heap, and `pointer` would store that memory's address.

Here's how that heap memory would be laid out on a 64-bit system. It's a total of 48 bytes.

```
|---8B---|---8B---|------16B------|------16B------|
 refcount  unused      string #1       string #2
```

Just like how `List` is a `struct` that takes up `2 * usize` bytes in memory, `Str` takes up the same amount of memory - namely, 16B on a 64-bit system. That's why each of the two strings take up 16B of this heap-allocated memory. (Those structs may also point to other heap memory, but they could also be empty strings! Either way we just store the structs in the list, which take up 16B.)

We'll get to what the refcount is for shortly, but first let's talk about the memory layout. The refcount is a `usize` integer, so 8B on our 64-bit system. Why is there 8B of unused memory after it?

This is because of memory alignment. Whenever a system loads some memory from a memory address, it's much more efficient if the address is a multiple of the number of bytes it wants to get. So if we want to load a 16B string struct, we want its address to be a multiple of 16.

When we're allocating memory on the heap, the way we specify what alignment we want is to say how big each element is, and how many of them we want. In this case, we say we want 16B elements, and we want 3 of them. Then we use the first 16B slot to store the 8B refcount, and the 8B after it are unused.

This is memory-inefficient, but it's the price we pay for having all the 16B strings stored in addresses that are multiples of 16. It'd be worse for performance if we tried to pack everything tightly, so we accept the memory inefficiency as a cost of achieving better overall execution speed.

> Note: if we happened to have 8B elements instead of 16B elements, the alignment would be 8 anyway and we'd have no unused memory.

## Reference counting

Let's go back to the refcount - short for "reference count."

The refcount is a `usize` integer which counts how many times this `List` has been shared. For example, if we named this list `myList` and then wrote `[ myList, myList, myList ]` then we'd increment that refcount 3 times because `myList` is now being shared three more times.

If we were to later call `List.pop` on that list, and the result was an in-place mutation that removed one of the `myList` entries, we'd decrement the refcount. If we did that again and again until the refcount got all the way down to 0, meaning nothing is using it anymore, then we'd deallocate these 48B of heap memory because nobody is using them anymore.

In some cases, the compiler can detect that no reference counting is necessary. In that scenario, it doesn't bother allocating extra space for the refcount; instead, it inserts an instruction to allocate the memory at the appropriate place, another to free it later, and that's it.

## Pointing to the first element

The fact that the reference count may or may not be present could creat a tricky situation for some `List` operations.

For example, should `List.get 0` return the first 16B of the heap-allocated bytes, or the second 16B? If there's a reference count in the first 16B, it should return the second 16B. If there's no refcount, it should return the first 16B. 

To solve this, the pointer in the List struct *always* points to the first element in the list. That means to access the reference count, it does negative pointer arithmetic to get the address at 16B *preceding* the memory address it has stored in its pointer field.

### Saturated reference count

What happens if the reference count overflows? As in, we try to reference the same list more than `usize` times?

In this situation, the reference count becomes unreliable. Suppose we try to increment it 3 more times after it's already hit `usize::MAX`, and since we can't store any higher numbers, we leave it at `usize::MAX`. If we later decrement it `usize::MAX` times, we'll be down to 0 and will free the memory, even though 3 things are still referencing that memory!

This would be a total disaster, so what we do instead is that we decide to leak the memory. Once the reference count hits `usize::MAX`, we neither increment nor decrement it ever again, which in turn means we will never free it.

This has the downside of being potentially wasteful of the program's memory, but it's less detrimental to user experience than a crash, and it doesn't impact correctness at all.

## Unique lists

If uniqueness typing tells us that a list is Unique, we know two things about it:

1. It doesn't need a refcount, because nothing else ever references it.
2. It can be mutated in-place.

One of the in-place mutations that can happen to a list is that its length can increase. For example, if I call `List.append list1 list2`, and `list1` is unique, then we'll attempt to append `list2`'s contents in-place into `list1`.

Calling `List.append` on a Shared list results in allocating a new chunk of heap memory large enough to hold both lists (with a fresh refcount, since nothing is referencing the new memory yet), then copying the contents of both lists into the new memory, and finally decrementing the refcount of the old memory.

Calling `List.append` on a Unique list can potentially be done in-place instead.

First, `List.append` repurposes the `usize` slot normally used to store refcount, and stores a `capacity` counter in there instead of a refcount. (After all, unique lists don't need to be refcounted.) A list's capacity refers to how many elements the list *can* hold given the memory it has allocated to it, which is always guaranteed to be at least as many as its length.

When calling `List.append list1 list2` on a unique `list1`, first we'll check to see if `list1.capacity <= list1.length + list2.length`. If it is, then we can copy in the new values without needing to allocate more memory for `list1`.

If there is not enough capacity to fit both lists, then we can try to call [`realloc`](https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/realloc?view=vs-2019) to hopefully extend the size of our allocated memory. If `realloc` succeeds (meaning there happened to be enough free memory right after our current allocation), then we update `capacity` to reflect the new amount of space, and move on.

If `realloc` fails, then we have to fall back on the same "allocate new memory and copy everything" strategy that we do with shared lists.
 
When you have a way to anticipate that a list will want to grow incrementally to a certain size, you can avoid such extra allocations by using `List.reserve` to guarantee more capacity up front. (`List.reserve` works like Rust's [`Vec::reserve`](https://doc.rust-lang.org/std/vec/struct.Vec.html#method.reserve).)

> **Note:** Calling `List.reserve 0 myList` will have no effect on a Unique list, but on a Shared list it will clone `myList` and return a Unique one. If you want to do a bunch of in-place mutations on a list, but it's currently Shared, calling `List.reserve 0` on it to get a Unique clone could actually be an effective performance optimization!

## Capacity and alignment

Some lists may end up beginning with excess capacity due to memory alignment requirements. Since the refcount is `usize`, all lists need a minimum of that alignment. For example, on a 64-bit system, a `List Bool` has an alignment of 8B even though bools can fit in 1B.

This means the list `[ True, True, False ]` would have a memory layout like this):

```
|--------------8B--------------|--1B--|--1B--|--1B--|-----5B-----|
  either refcount or capacity   bool1  bool2  bool3     unused
```

As such, if this list is Unique, it would start out with a length of 3 and a capacity of 8.

Since each bool value is a byte, it's okay for them to be packed side-by-side even though the overall alignment of the list elements is 8. This is fine because each of their individual memory addresses will end up being a multiple of their size in bytes.

Note that unlike in the `List Str` example before, there wouldn't be any unused memory between the refcount (or capacity, depending on whether the list was shared or unique) and the first element in the list. That will always be the case when the size of the refcount is no bigger than the alignment of the list's elements.

## Summary of Lists

Lists are a `2 * usize` struct which contains a length and a pointer.

That pointer is a memory address (null in the case of an empty list) which points to the first element in a sequential array of memory.

If that pointer is shared in multiple places, then there will be a `usize` reference count stored right before the first element of the list. There may be unused memory after the refcount if `usize` is smaller than the alignment of one of the list's elements.

Refcounts get incremented each time a list gets shared somewhere, and decremented each time that shared value is no longer referenced by anything else (for example, by going out of scope). Once there are no more references, the list's heap memory can be safely freed. If a reference count gets all the way up to `usize`, then it will never be decremented again and the memory will never be freed.

Whenever a list grows, it will grow in-place if it's Unique and there is enough capacity. (Capacity is stored where a refcount would be in a Shared list.) If there isn't enough capacity - even after trying `realloc` - or if the list is Shared, then instead new heap memory will be allocated, all the necessary elements will get copied into it, and the original list's refcount will be decremented.

## Strings

Strings have several things in common with lists:

* They are a `2 * usize` struct, sometimes with a non-null pointer to some heap memory
* They have a length and a capacity, and they can grow in basically the same way
* They are reference counted in basically the same way

However, they also have two things going on that lists do not:

* The Small String Optimization
* Literals stored in read-only memory

## The Small String Optimization

In practice, a lot of strings are pretty small. For example, the string `"Richard Feldman"` can be stored in 15 UTF-8 bytes. If we stored that string the same way we store a list, then on a 64-bit system we'd need a 16B struct, which would include a pointer to 24B of heap memory (including the refcount/capacity and one unused byte for alignment).

That's a total of 48B to store 15B of data, when we could have fit the whole string into the original 16B we needed for the struct, with one byte left over.

The Small String Optimization is where we store strings directly in the struct, assuming they can fit in there. We reserve one of those bytes to indicate whether this is a Small String or a larger one that actually uses a pointer.

## String Memory Layout

How do we tell small strings apart from nonsmall strings?

We make use of the fact that lengths (for both strings *and* lists) are `usize` values which have a maximum value of `isize::MAX` rather than `usize::MAX`. This is because `List.get` compiles down to an array access operation, and LLVM uses `isize` indices for those because they do signed arithmetic on the pointer in case the caller wants to add a negative number to the address. (We don't want to, as it happens, but that's what the low-level API supports, so we are bound by its limitations.)

Since the string's length is a `usize` value with a maximum of `isize::MAX`, we can be sure that its most significant bit will always be 0, not 1. (If it were a 1, that would be a negative `isize`!) We can use this fact to use that spare bit as a flag indicating whether the string is small: if that bit is a 1, it's a small string; otherwise, it's a nonsmall string.

This makes calculating the length of the string a multi-step process:

1. Get the length field out of the struct.
2. Look at its highest bit. If that bit is 0, return the length as-is.
3. If the bit is 1, then this is a small string, and its length is packed into the highest byte of the `usize` length field we're currently examining. Take that byte and bit shift it by 1 (to drop the `1` flag we used to indicate this is a small string), cast the resulting byte to `usize`, and that's our length.

Using this strategy with a [conditional move instruction](https://stackoverflow.com/questions/14131096/why-is-a-conditional-move-not-vulnerable-for-branch-prediction-failure), we can always get the length of a `Str` in 2-3 cheap instructions on a single `usize` value, without any chance of a branch misprediction.

Thus, the layout of a small string on a 64-bit big-endian architecture would be:

```
|-----------usize length field----------|-----------usize pointer field---------|
|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|
 len   'R'  'i'  'c'  'h'  'a'  'r'  'd'  ' '  'F'  'e'  'l'  'd'  'm'  'a'  'n'
```

The `len` value here would be the number 15, plus a 1 (to flag that this is a small string) that would always get bit-shifted away. The capacity of a small Unique string is always equal to `2 * usize`, because that's how much you can fit without promoting to a nonsmall string.

## Endianness

The preceding memory layout example works on a big-endian architecture, but most CPUs are little-endian. That means the high bit where we want to store the flag (the 0 or 1
that would make an `isize` either negative or positive) will actually be the `usize`'s last byte rather than its first byte.

That means we'd have to move swap the order of the struct's length and pointer fields. Here's how the string `"Roc string"` would be stored on a little-endian system:

```
|-----------usize pointer field---------|-----------usize length field----------|
|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|-1B-|
  'R'  'o'  'c'  ' '  's'  't'  'r'  'i'  'n'  'g'   0    0    0    0    0   len
```

Here, `len` would have the same format as before (including the extra 1 in the same position, which we'd bit shift away) except that it'd store a length of 10 instead of 15.

Notice that the leftover bytes are stored as zeroes. This is handy because it means we can convert small Roc strings into C strings (which are 0-terminated) for free as long as they have at least one unused byte. Also notice that `usize pointer field` and `usize length field` have been swapped compared to the preceding example!

## Storing string literals in read-only memory

