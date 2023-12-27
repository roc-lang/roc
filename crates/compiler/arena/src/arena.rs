use core::{alloc::Layout, ptr::NonNull};

use bumpalo::Bump;

/// Like [Bumpalo](https://docs.rs/bumpalo/3.14.0/bumpalo) but with some changes:
/// * Never panics; all APIs return `Result` so callers must handle the possibility of allocation failure

// This is adapted from https://github.com/fitzgen/bumpalo version 3.14 under
// the Apache License, Version 2.0.
//
// https://github.com/fitzgen/bumpalo/blob/c0b113775078c3daabea2de63beb05401a480e68/LICENSE-APACHE
//
// Thank you, Nick Fitzgerald and other Bumpalo contributors!

#[repr(transparent)]
pub struct Arena {
    bump: Bump,
}

pub type AllocErr = bumpalo::AllocErr;
pub type AllocOrInitError<T> = bumpalo::AllocOrInitError<T>;

impl Arena {
    pub fn alloc<T>(&self, val: T) -> Result<&mut T, AllocErr> {
        self.bump.try_alloc(val)
    }

    pub fn alloc_layout(&self, layout: Layout) -> Result<NonNull<u8>, AllocErr> {
        self.bump.try_alloc_layout(layout)
    }

    /// Tries to pre-allocates space for a [`Result`] in this arena,
    /// initializes it using the closure, then returns an exclusive reference
    /// to its `T` if all [`Ok`].
    ///
    /// Iff the allocation fails, the closure is not run.
    ///
    /// Iff the closure returns [`Err`], an allocator rewind is *attempted* and
    /// the `E` instance is moved out of the allocator to be consumed or dropped
    /// as normal.
    ///
    /// See [The `_with` Method Suffix](#initializer-functions-the-_with-method-suffix) for a
    /// discussion on the differences between the `_with` suffixed methods and
    /// those methods without it, their performance characteristics, and when
    /// you might or might not choose a `_with` suffixed method.
    ///
    /// For caveats specific to fallible initialization, see
    /// [The `_try_with` Method Suffix](#fallible-initialization-the-_try_with-method-suffix).
    ///
    /// [`Result`]: https://doc.rust-lang.org/std/result/enum.Result.html
    /// [`Ok`]: https://doc.rust-lang.org/std/result/enum.Result.html#variant.Ok
    /// [`Err`]: https://doc.rust-lang.org/std/result/enum.Result.html#variant.Err
    ///
    /// ## Errors
    ///
    /// Errors with the [`Alloc`](`AllocOrInitError::Alloc`) variant iff
    /// reserving space for `Result<T, E>` fails.
    ///
    /// Iff the allocation succeeds but `f` fails, that error is forwarded by
    /// value inside the [`Init`](`AllocOrInitError::Init`) variant.
    ///
    /// ## Example
    ///
    /// ```
    /// let arena = arena::Arena::new();
    /// let x = arena.alloc_with(|| Ok("hello"))?;
    /// assert_eq!(*x, "hello");
    /// # Result::<_, arena::AllocOrInitError<()>>::Ok(())
    /// ```
    #[inline(always)]
    #[allow(clippy::mut_from_ref)]
    pub fn alloc_with<F, T, E>(&self, f: F) -> Result<&mut T, AllocOrInitError<E>>
    where
        F: FnOnce() -> Result<T, E>,
    {
        self.bump.try_alloc_try_with(f)
    }

    /// This is unsafe because there could still be collections
    /// which reference this memory. If this is called while there
    /// are still live collections which were allocated using this
    /// arena, the result will be undefined behavior.
    pub unsafe fn reset(&mut self) {
        self.bump.reset()
    }
}
