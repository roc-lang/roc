#[derive(Clone, Debug, PartialEq)]
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

    pub fn contains(&self, value: &T) -> bool {
        self.elements.contains(value)
    }

    pub fn remove(&mut self, value: &T) {
        match self.elements.iter().position(|x| x == value) {
            None => {
                // just do nothing
            }
            Some(index) => {
                self.elements.swap_remove(index);
            }
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.elements.iter()
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

impl<T> IntoIterator for VecSet<T> {
    type Item = T;

    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.elements.into_iter()
    }
}
