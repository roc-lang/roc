#[derive(Debug, Clone)]
pub struct VecMap<K, V> {
    keys: Vec<K>,
    values: Vec<V>,
}

impl<K, V> Default for VecMap<K, V> {
    fn default() -> Self {
        Self {
            keys: Vec::new(),
            values: Vec::new(),
        }
    }
}

impl<K, V> VecMap<K, V> {
    pub fn len(&self) -> usize {
        debug_assert_eq!(self.keys.len(), self.values.len());
        self.keys.len()
    }
}

impl<K: PartialEq, V> VecMap<K, V> {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            keys: Vec::with_capacity(capacity),
            values: Vec::with_capacity(capacity),
        }
    }

    pub fn is_empty(&self) -> bool {
        debug_assert_eq!(self.keys.len(), self.values.len());
        self.keys.is_empty()
    }

    pub fn swap_remove(&mut self, index: usize) -> (K, V) {
        let k = self.keys.swap_remove(index);
        let v = self.values.swap_remove(index);

        (k, v)
    }

    pub fn insert(&mut self, key: K, mut value: V) -> Option<V> {
        match self.keys.iter().position(|x| x == &key) {
            Some(index) => {
                std::mem::swap(&mut value, &mut self.values[index]);

                Some(value)
            }
            None => {
                self.keys.push(key);
                self.values.push(value);

                None
            }
        }
    }

    pub fn contains_key(&self, key: &K) -> bool {
        self.keys.contains(key)
    }

    pub fn remove(&mut self, key: &K) -> Option<(K, V)> {
        let index = self.keys.iter().position(|x| x == key)?;
        Some(self.swap_remove(index))
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        match self.keys.iter().position(|x| x == key) {
            None => None,
            Some(index) => Some(&self.values[index]),
        }
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        match self.keys.iter().position(|x| x == key) {
            None => None,
            Some(index) => Some(&mut self.values[index]),
        }
    }

    pub fn get_or_insert(&mut self, key: K, default_value: impl FnOnce() -> V) -> &mut V {
        match self.keys.iter().position(|x| x == &key) {
            Some(index) => &mut self.values[index],
            None => {
                let value = default_value();

                self.keys.push(key);
                self.values.push(value);

                self.values.last_mut().unwrap()
            }
        }
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = (&K, &V)> {
        self.keys.iter().zip(self.values.iter())
    }

    pub fn keys(&self) -> impl ExactSizeIterator<Item = &K> {
        self.keys.iter()
    }

    pub fn values(&self) -> impl ExactSizeIterator<Item = &V> {
        self.values.iter()
    }

    pub fn truncate(&mut self, len: usize) {
        self.keys.truncate(len);
        self.values.truncate(len);
    }

    pub fn unzip(self) -> (Vec<K>, Vec<V>) {
        (self.keys, self.values)
    }

    /// # Safety
    ///
    /// keys and values must have the same length, and there must not
    /// be any duplicates in the keys vector
    pub unsafe fn zip(keys: Vec<K>, values: Vec<V>) -> Self {
        Self { keys, values }
    }
}

impl<K: Ord, V> Extend<(K, V)> for VecMap<K, V> {
    #[inline(always)]
    fn extend<T: IntoIterator<Item = (K, V)>>(&mut self, iter: T) {
        let it = iter.into_iter();
        let hint = it.size_hint();

        match hint {
            (0, Some(0)) => {
                // done, do nothing
            }
            (1, Some(1)) | (2, Some(2)) => {
                for (k, v) in it {
                    self.insert(k, v);
                }
            }
            (_min, _opt_max) => {
                // TODO do this with sorting and dedup?
                for (k, v) in it {
                    self.insert(k, v);
                }
            }
        }
    }
}

impl<K, V> IntoIterator for VecMap<K, V> {
    type Item = (K, V);

    type IntoIter = IntoIter<K, V>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            len: self.len(),
            keys: self.keys.into_iter(),
            values: self.values.into_iter(),
        }
    }
}

pub struct IntoIter<K, V> {
    len: usize,
    keys: std::vec::IntoIter<K>,
    values: std::vec::IntoIter<V>,
}

impl<K, V> Iterator for IntoIter<K, V> {
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        match (self.keys.next(), self.values.next()) {
            (Some(k), Some(v)) => Some((k, v)),
            _ => None,
        }
    }
}

impl<K, V> ExactSizeIterator for IntoIter<K, V> {
    fn len(&self) -> usize {
        self.len
    }
}
