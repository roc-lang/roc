use core::{fmt, marker::PhantomData, mem::MaybeUninit, ptr, usize};

use crate::Arena;

/// A mutable reference to something that has been allocated inside an Arena.
///
/// Importantly, it's stored as a byte offset into the arena's memory,
/// which means it can be serialized to/from disk and still work.
///
/// This also means that dereferencing it requires passing in the arena
/// where it was originally allocated. In debug builds, dereferencing will
/// do a check to make sure the arena being passed in is the same one that
/// was originally used to allocate the reference. (If not, it will panic.)
/// In release builds, this information is stored and nothing is checked at runtime.
pub struct ArenaRefMut<'a, T> {
    byte_offset_into_arena: u32,
    _marker: PhantomData<&'a T>,

    #[cfg(debug_assertions)]
    pub(crate) arena: &'a Arena<'a>,
}

impl<'a, T: fmt::Debug> fmt::Debug for ArenaRefMut<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let arg;

        #[cfg(debug_assertions)]
        {
            arg = self.as_ref(self.arena).fmt(f);
        }

        #[cfg(not(debug_assertions))]
        {
            arg = self.byte_offset_into_arena;
        }

        write!(f, "ArenaRefMut({:?})", arg)
    }
}

impl<'a, T> ArenaRefMut<'a, MaybeUninit<T>> {
    pub const unsafe fn assume_init(self) -> ArenaRefMut<'a, T> {
        self.cast()
    }
}

impl<'a, T> ArenaRefMut<'a, T> {
    pub const unsafe fn add_bytes(&self, amount: u32) -> Self {
        Self {
            byte_offset_into_arena: self.byte_offset_into_arena + amount,
            _marker: PhantomData,

            #[cfg(debug_assertions)]
            arena: self.arena,
        }
    }

    pub(crate) const fn new_in(byte_offset_into_arena: u32, _arena: &'a Arena<'a>) -> Self {
        Self {
            byte_offset_into_arena,
            _marker: PhantomData,
            #[cfg(debug_assertions)]
            arena: _arena,
        }
    }

    pub(crate) const fn byte_offset(&self) -> usize {
        self.byte_offset_into_arena as usize
    }

    pub fn as_ref(&self, arena: &Arena<'a>) -> &T {
        #[cfg(debug_assertions)]
        {
            self.debug_verify_arena(arena, "ArenaRefMut::deref");
        }

        unsafe {
            &*(arena.content as *const _ as *const u8)
                .add(self.byte_offset())
                .cast()
        }
    }

    pub fn as_mut(&mut self, arena: &mut Arena<'a>) -> &mut T {
        #[cfg(debug_assertions)]
        {
            self.debug_verify_arena(arena, "ArenaRefMut::deref");
        }

        unsafe {
            &mut *(arena.content as *mut _ as *mut u8)
                .add(self.byte_offset())
                .cast()
        }
    }

    pub const unsafe fn cast<U>(self) -> ArenaRefMut<'a, U> {
        core::mem::transmute::<ArenaRefMut<'a, T>, ArenaRefMut<'a, U>>(self)
    }

    #[cfg(debug_assertions)]
    pub fn debug_verify_arena(&self, other_arena: &Arena<'a>, fn_name: &'static str) {
        // This only does anything in debug builds. In optimized builds, we don't do it.
        if (self.arena as *const _) != (other_arena as *const _) {
            panic!("{fn_name} was called passing a different arena from the one this ArenaRefMut was created with!");
        }
    }
}

impl<'a, T: Copy> ArenaRefMut<'a, T> {
    pub fn deref(&self, arena: &Arena<'a>) -> T {
        #[cfg(debug_assertions)]
        {
            self.debug_verify_arena(arena, "deref");
        }

        unsafe {
            ptr::read(
                (arena.content as *const _ as *const u8)
                    .add(self.byte_offset())
                    .cast(),
            )
        }
    }
}

/////////////////////////////////////////////////////////////////////////////////////////////////////

/// An immutable reference to something that has been allocated inside an Arena.
///
/// Importantly, it's stored as a byte offset into the arena's memory,
/// which means it can be serialized to/from disk and still work.
///
/// This also means that dereferencing it requires passing in the arena
/// where it was originally allocated. In debug builds, dereferencing will
/// do a check to make sure the arena being passed in is the same one that
/// was originally used to allocate the reference. (If not, it will panic.)
/// In release builds, this information is stored and nothing is checked at runtime.
#[cfg_attr(not(debug_assertions), repr(transparent))]
#[derive(Copy, Clone)]
pub struct ArenaRef<'a, T> {
    byte_offset_into_arena: u32,
    _marker: PhantomData<&'a T>,

    #[cfg(debug_assertions)]
    pub(crate) arena: &'a Arena<'a>,
}

impl<'a, T: fmt::Debug> fmt::Debug for ArenaRef<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let arg;

        #[cfg(debug_assertions)]
        {
            arg = self.as_ref(self.arena).fmt(f);
        }

        #[cfg(not(debug_assertions))]
        {
            arg = self.byte_offset_into_arena;
        }

        write!(f, "ArenaRef({:?})", arg)
    }
}

impl<'a, T> ArenaRef<'a, MaybeUninit<T>> {
    pub const unsafe fn assume_init(self) -> ArenaRef<'a, T> {
        self.cast()
    }
}

impl<'a, T> ArenaRef<'a, T> {
    pub(crate) const fn _new_in(byte_offset_into_arena: u32, _arena: &'a Arena<'a>) -> Self {
        Self {
            byte_offset_into_arena,
            _marker: PhantomData,
            #[cfg(debug_assertions)]
            arena: _arena,
        }
    }

    pub(crate) const fn byte_offset(&self) -> usize {
        self.byte_offset_into_arena as usize
    }

    pub(crate) const fn _add_bytes(&self, amount: u32) -> Self {
        Self {
            byte_offset_into_arena: self.byte_offset_into_arena + amount,
            _marker: PhantomData,

            #[cfg(debug_assertions)]
            arena: self.arena,
        }
    }

    pub fn as_ref(&self, arena: &Arena<'a>) -> &'a T {
        #[cfg(debug_assertions)]
        {
            self.debug_verify_arena(arena, "ArenaRef::deref");
        }

        unsafe {
            &*(arena.content as *const _ as *const u8)
                .add(self.byte_offset())
                .cast()
        }
    }

    pub(crate) const fn cast<U>(self) -> ArenaRef<'a, U> {
        unsafe { core::mem::transmute::<ArenaRef<'a, T>, ArenaRef<'a, U>>(self) }
    }

    #[cfg(debug_assertions)]
    pub(crate) fn debug_verify_arena(&self, other_arena: &Arena<'a>, fn_name: &'static str) {
        // This only does anything in debug builds. In optimized builds, we don't do it.
        if (self.arena as *const _) != (other_arena as *const _) {
            panic!("{fn_name} was called passing a different arena from the one this ArenaRef was created with!");
        }
    }
}

impl<'a, T: Copy> ArenaRef<'a, T> {
    pub fn deref(&self, arena: &Arena<'a>) -> T {
        #[cfg(debug_assertions)]
        {
            self.debug_verify_arena(arena, "deref");
        }

        unsafe {
            ptr::read(
                (arena.content as *const _ as *const u8)
                    .add(self.byte_offset())
                    .cast(),
            )
        }
    }
}

impl<'a, T> From<ArenaRefMut<'a, T>> for ArenaRef<'a, T> {
    fn from(value: ArenaRefMut<'a, T>) -> Self {
        Self {
            byte_offset_into_arena: value.byte_offset_into_arena,
            _marker: PhantomData,
            #[cfg(debug_assertions)]
            arena: value.arena,
        }
    }
}
