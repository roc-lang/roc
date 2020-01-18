// Adapted from the symbol-map crate by Donald S. Black, licensed using the
// Apache License, Version 2.0. Thank you, Donald!
//
// Indexing on top of a `Table`.
//
// It is anticipated that most uses cases will be covered by
// [HashIndexing](struct.HashIndexing.html), which owns a
// [Table](../struct.Table.html) and provides bidirectional mappings between
// data values and their symbols.
//
// The [Indexing](trait.Indexing.html) trait is provided in case another lookup
// method is needed.

use crate::collections::{default_hasher, BuildHasher};
use std::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};
use std::collections::HashMap;
use std::default::Default;
use std::fmt;
use std::hash::{Hash, Hasher};

use symbol_map::{Symbol, SymbolId, Table};

/// Indicates whether the result of a symbol lookup had to create a new table
/// entry.
#[derive(Clone, Eq, Ord, Hash, PartialEq, PartialOrd)]
pub enum Insertion<T> {
    /// Result came from an item that was already present in table.
    Present(T),
    /// Result came from an item that was not present in table, and a new entry
    /// was created.
    New(T),
}

impl<T> Insertion<T> {
    /// Maps over the type returned by an `Insertion` to produce a new value
    /// that may be of a different type.
    ///
    /// # Example
    ///
    /// use symbol_map::indexing::{HashIndexing, Indexing, Insertion};
    /// use std::str::FromStr;
    ///
    /// let mut index = HashIndexing::<String, usize>::default();
    /// let s1 = String::from_str("value1").unwrap();
    /// let s2 = String::from_str("value1").unwrap();
    /// let s3 = String::from_str("value2").unwrap();
    /// // get_or_insert normally returns an Insertion that borrows the
    /// // structure on which it was invoked. We map the symbol reference
    /// // returned after each insertion to a copy of the ID that was mapped to.
    /// let id1: Insertion<usize> = index.get_or_insert(s1).map(|symbol| *symbol.id());
    /// let id2: Insertion<usize> = index.get_or_insert(s2).map(|symbol| *symbol.id());
    /// let id3: Insertion<usize> = index.get_or_insert(s3).map(|symbol| *symbol.id());
    /// // The Insertion values are not the same because one was an insertion and
    /// // the other a retrieval.
    /// assert!(id1 != id2);
    /// assert!(id1 != id3);
    /// // But the symbol IDs for identical values are the same.
    /// assert!(id1.unwrap() == id2.unwrap());
    pub fn map<F, X>(&self, f: F) -> Insertion<X>
    where
        F: FnOnce(&T) -> X,
    {
        match *self {
            Insertion::Present(ref s) => Insertion::Present(f(s)),
            Insertion::New(ref s) => Insertion::New(f(s)),
        }
    }

    /// Unwraps an `Insertion` to produce the value which it wraps.
    pub fn unwrap(self) -> T {
        match self {
            Insertion::Present(s) => s,
            Insertion::New(s) => s,
        }
    }
}

/// Wrapper for a raw pointer which lets us treat it like a reference.
///
/// You are strongly discouraged from exposing this type directly in your data
/// structures. This type is essentially a giant footgun. In particular:
///
/// - No safety checks or lifetimes protect this reference, so a `Ref<T>` may be
/// invalidated without warning. (You may use a `Ref<T>` safely by ensuring that
/// the references passed to `Ref<T>::new()` will never be dropped before the
/// wrappers. A good example of when you'd be able to do this is in in a struct
/// that has `Ref<T>` references into a data structure that it also owns.)
///
/// - The impls for `Debug`, `Eq`, `Hash`, `Ord`, `PartialEq`, and `PartialOrd`
/// all dereference the raw pointer that this structure wraps. As a result, a
/// `Ref<T>` must be removed from any data structures that make use of any of
/// those interfaces *before* it is invalidated.
///
/// - `Ref<T>` wraps a value of type `*const T`, which is not usually `Send` or
/// `Sync`. This restriction is overridden for a `Ref<T>` wrapper so that data
/// structures which encapsulate it may themselves be `Send` or `Sync`. This
/// makes it the responsibility of data structures using such wrappers to
/// satisfy the contracts of those types.
pub struct Ref<T> {
    ptr: *const T,
}

unsafe impl<T> Send for Ref<T> where T: Send {}

unsafe impl<T> Sync for Ref<T> where T: Sync {}

impl<T> Ref<T> {
    /// Casts `data` to `*const T` and retains the pointer for dereferencing at
    /// some point in the future.
    fn new(data: &T) -> Self {
        Ref {
            ptr: data as *const T,
        }
    }

    /// Dereferences the wrapped pointer. The explicit lifetime parameter should
    /// match the lifetime of the parent `Table` that the wrapped pointer points
    /// into. Care should be taken not to call this method if the integrity of
    /// the reference passed to `new()` cannot be verified.
    unsafe fn deref<'a>(self) -> &'a T {
        &*self.ptr
    }
}

impl<T> Clone for Ref<T> {
    fn clone(&self) -> Self {
        Ref { ptr: self.ptr }
    }
}

impl<T> Copy for Ref<T> {}

impl<T> fmt::Pointer for Ref<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Pointer::fmt(&self.ptr, f)
    }
}

impl<T> fmt::Debug for Ref<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Ref({:?})", unsafe { &(*self.ptr) })
    }
}

impl<T> Eq for Ref<T> where T: Eq {}

impl<T> Hash for Ref<T>
where
    T: Hash,
{
    fn hash<H>(&self, h: &mut H)
    where
        H: Hasher,
    {
        unsafe { (*self.ptr).hash(h) }
    }
}

impl<T> Ord for Ref<T>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        unsafe { (*self.ptr).cmp(&(*other.ptr)) }
    }
}

impl<T> PartialEq for Ref<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        unsafe { (*self.ptr).eq(&(*other.ptr)) }
    }
}

impl<T> PartialOrd for Ref<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        unsafe { (*self.ptr).partial_cmp(&(*other.ptr)) }
    }
}

/// Provides indexing for a `Table`, so that its elements may be retrieved
/// efficiently. Most table lookups should go through an implementation of this
/// trait structure instead of a `Table` directly.
///
/// An `Indexing` should own an underlying `Table<Indexing::Data>`. This table
/// provides persistent storage for `Symbol<Indexing::Data>`s, which associate
/// instances of `Data` with a `SymbolId`.
///
/// This trait is provided for extensibility. Realistically speaking, however,
/// you should probably just use `HashIndexing`.
pub trait Indexing: Default {
    /// The type `T` of a `Table<T, D>`.
    type Data;

    /// The type `D` of a `Table<T, D>`.
    type Id: SymbolId;

    /// Returns a new indexing method that has already indexed the contents of
    /// `table`.
    fn from_table(table: Table<Self::Data, Self::Id>) -> Self;

    /// Returns a read-only view of the underlying table.
    fn table(&self) -> &Table<Self::Data, Self::Id>;

    /// Extracts the underlying table from the index, discarding all pointers
    /// into the table.
    fn to_table(self) -> Table<Self::Data, Self::Id>;

    /// Looks up `data` in the index. Returns `Some(&symbol)` if a symbol is
    /// present, else `None`.
    fn get(&self, data: &Self::Data) -> Option<&Symbol<Self::Data, Self::Id>>;

    /// Looks up `data` in the index, inserting it into the index and `table` if
    /// it isn't present. Returns the resulting `&Symbol<T>` wrapped in an
    /// `Insertion` that indicates whether a new table entry had to be created.
    fn get_or_insert<'s>(
        &'s mut self,
        data: Self::Data,
    ) -> Insertion<&'s Symbol<Self::Data, Self::Id>>;

    /// Looks up the symbol with id `i` in the index. Returns `Some(symbol)` if
    /// a symbol is present, else `None`.
    fn get_symbol<'s>(&'s self, id: &Self::Id) -> Option<&'s Symbol<Self::Data, Self::Id>>;
}

/// HashMap-backed table indexing.
pub struct HashIndexing<T, D>
where
    T: Eq + Hash,
    D: SymbolId,
{
    table: Table<T, D>,
    by_symbol: HashMap<Ref<T>, Ref<Symbol<T, D>>, BuildHasher>,
    by_id: Vec<Ref<Symbol<T, D>>>,
}

impl<T, D> Default for HashIndexing<T, D>
where
    T: Eq + Hash,
    D: SymbolId,
{
    fn default() -> Self {
        HashIndexing {
            table: Table::new(),
            by_symbol: HashMap::with_hasher(default_hasher()),
            by_id: Vec::new(),
        }
    }
}

impl<T, D> Indexing for HashIndexing<T, D>
where
    T: Eq + Hash,
    D: SymbolId,
{
    type Data = T;
    type Id = D;

    fn from_table(table: Table<T, D>) -> Self {
        let mut by_symbol = HashMap::with_capacity_and_hasher(table.len(), default_hasher());
        let mut by_id = match table.iter().next() {
            Some(symbol) => vec![Ref::new(symbol); table.len()],
            None => Vec::new(),
        };
        for symbol in table.iter() {
            by_symbol.insert(Ref::new(symbol.data()), Ref::new(symbol));
            by_id[symbol.id().as_usize()] = Ref::new(symbol);
        }
        HashIndexing {
            table,
            by_symbol,
            by_id,
        }
    }

    fn table(&self) -> &Table<Self::Data, Self::Id> {
        &self.table
    }

    fn to_table(self) -> Table<Self::Data, Self::Id> {
        self.table
    }

    fn get<'s>(&'s self, data: &T) -> Option<&'s Symbol<T, D>> {
        // Unsafe call to Ref::deref(): should be fine as because we own
        // self.table and the ref refers into that.
        self.by_symbol
            .get(&Ref::new(data))
            .map(|x| unsafe { x.deref() })
    }

    fn get_or_insert<'s>(&'s mut self, data: T) -> Insertion<&'s Symbol<T, D>> {
        use std::collections::hash_map::Entry;
        if let Entry::Occupied(e) = self.by_symbol.entry(Ref::new(&data)) {
            // Unsafe call to Ref::deref(): should be fine as because we own
            // self.table and the ref refers into that.
            return Insertion::Present(unsafe { e.get().deref() });
        }
        // TODO: when the HashMap API gets revised, we may be able to do this
        // without a second hashtable lookup.
        let symbol = self.table.insert(data);
        // The Ref that gets inserted has to be backed by data in the table, not
        // data on the stack (which is how we did the previous lookup).
        self.by_symbol
            .insert(Ref::new(symbol.data()), Ref::new(symbol));
        self.by_id.push(Ref::new(symbol));
        Insertion::New(symbol)
    }

    fn get_symbol<'s>(&'s self, id: &D) -> Option<&'s Symbol<T, D>> {
        self.by_id.get(id.as_usize()).map(|x| unsafe { x.deref() })
    }
}
