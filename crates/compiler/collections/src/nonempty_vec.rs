pub struct NonEmptyVec<T> {
    first: T,
    rest: Vec<T>,
}

impl<T> NonEmptyVec<T> {
    pub fn singleton(item: T) -> Self {
        Self::singleton_with_capacity(item, 0)
    }

    pub fn singleton_with_capacity(item: T, capacity: usize) -> Self {
        Self {
            first: item,
            rest: Vec::with_capacity(capacity),
        }
    }

    pub fn len(&self) -> usize {
        self.rest.len() + 1
    }

    pub fn is_singleton(&self) -> bool {
        self.rest.is_empty()
    }

    pub fn push(&mut self, item: T) {
        self.rest.push(item);
    }

    /// Pops the last item. Returns [None] if that would pop the first item.
    pub fn pop(&mut self) -> Option<T> {
        self.rest.pop()
    }

    pub fn last(&mut self) -> &T {
        self.rest.last().unwrap_or(&self.first)
    }

    pub fn last_mut(&mut self) -> &mut T {
        self.rest.last_mut().unwrap_or(&mut self.first)
    }
}
