use std::{borrow::Borrow, iter::FromIterator};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VecSet<T> {
    elements: Vec<T>,
}

impl<T> Default for VecSet<T> {
    fn default() -> Self {
        Self {
            elements: Vec::new(),
        }
    }
}

impl<T> VecSet<T> {
    pub fn into_vec(self) -> Vec<T> {
        self.elements
    }

    pub fn reserve(&mut self, additional: usize) {
        self.elements.reserve(additional)
    }
}

impl<T: PartialEq> VecSet<T> {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            elements: Vec::with_capacity(capacity),
        }
    }

    pub fn len(&self) -> usize {
        self.elements.len()
    }

    pub fn is_empty(&self) -> bool {
        self.elements.is_empty()
    }

    pub fn singleton(value: T) -> Self {
        Self {
            elements: vec![value],
        }
    }

    pub fn swap_remove(&mut self, index: usize) -> T {
        self.elements.swap_remove(index)
    }

    pub fn insert(&mut self, value: T) -> bool {
        if self.elements.contains(&value) {
            true
        } else {
            self.elements.push(value);

            false
        }
    }

    /// Returns true iff any of the given elements previoously existed in the set.
    pub fn insert_all<I: Iterator<Item = T>>(&mut self, values: I) -> bool {
        let mut any_existed = false;

        for value in values {
            any_existed = any_existed || self.insert(value);
        }

        any_existed
    }

    pub fn contains(&self, value: &T) -> bool {
        self.elements.contains(value)
    }

    /// Performs a swap_remove if the element was present in the set,
    /// then returns whether the value was present in the set.
    pub fn remove(&mut self, value: &T) -> bool {
        match self.elements.iter().position(|x| x == value) {
            None => false,
            Some(index) => {
                self.elements.swap_remove(index);

                true
            }
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.elements.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.elements.iter_mut()
    }

    /// Removes all elements from the set, without affecting its allocated capacity.
    pub fn clear(&mut self) {
        self.elements.clear()
    }

    /// Retains only the elements specified by the predicate.
    pub fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&T) -> bool,
    {
        self.elements.retain(f)
    }
}

impl<A: Ord> Extend<A> for VecSet<A> {
    fn extend<T: IntoIterator<Item = A>>(&mut self, iter: T) {
        let it = iter.into_iter();
        let hint = it.size_hint();

        match hint {
            (0, Some(0)) => {
                // done, do nothing
            }
            (1, Some(1)) | (2, Some(2)) => {
                for value in it {
                    self.insert(value);
                }
            }
            _ => {
                self.elements.extend(it);

                self.elements.sort();
                self.elements.dedup();
            }
        }
    }
}

impl<A: Ord> FromIterator<A> for VecSet<A> {
    fn from_iter<T: IntoIterator<Item = A>>(iter: T) -> Self {
        let mut set = VecSet::default();
        set.extend(iter);
        set
    }
}

impl<T> IntoIterator for VecSet<T> {
    type Item = T;

    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.elements.into_iter()
    }
}

impl<T> Borrow<[T]> for VecSet<T> {
    fn borrow(&self) -> &[T] {
        &self.elements
    }
}

impl<T> From<Vec<T>> for VecSet<T> {
    fn from(elements: Vec<T>) -> Self {
        // Not totally safe, but good enough for our purposes - also, duplicates in the VecSet are
        // fine, just inefficient.
        Self { elements }
    }
}
