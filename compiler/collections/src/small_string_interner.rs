/// Collection of small (length < u16::MAX) strings, stored compactly.
#[derive(Clone, Default, PartialEq, Eq)]
pub struct SmallStringInterner {
    buffer: Vec<u8>,

    // lengths could be Vec<u8>, but the mono refcount generation
    // stringifies Layout's and that creates > 256 character strings
    lengths: Vec<u16>,
    offsets: Vec<u32>,
}

impl std::fmt::Debug for SmallStringInterner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let strings: Vec<_> = self.iter().collect();

        f.debug_struct("SmallStringInterner")
            .field("buffer", &self.buffer)
            .field("lengths", &self.lengths)
            .field("offsets", &self.offsets)
            .field("strings", &strings)
            .finish()
    }
}

impl SmallStringInterner {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            // guess: the average symbol length is 5
            buffer: Vec::with_capacity(5 * capacity),

            lengths: Vec::with_capacity(capacity),
            offsets: Vec::with_capacity(capacity),
        }
    }

    pub const fn from_parts(buffer: Vec<u8>, lengths: Vec<u16>, offsets: Vec<u32>) -> Self {
        Self {
            buffer,
            lengths,
            offsets,
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &str> {
        (0..self.offsets.len()).map(move |index| self.get(index))
    }

    pub fn insert(&mut self, string: &str) -> usize {
        let bytes = string.as_bytes();

        assert!(bytes.len() < u16::MAX as usize);

        let offset = self.buffer.len() as u32;
        let length = bytes.len() as u16;

        let index = self.lengths.len();

        self.lengths.push(length);
        self.offsets.push(offset);

        self.buffer.extend(bytes);

        index
    }

    /// Insert a string equal to the current length into the interner.
    ///
    /// Assuming that normally you don't insert strings consisting of just digits,
    /// this is an easy way to create a unique string name. We use this to create
    /// unique variable names: variable names cannot start with a digit in the source,
    /// so if we insert the current length of `length` as its digits, that is always unique
    pub fn insert_index_str(&mut self) -> usize {
        use std::io::Write;

        let index = self.lengths.len();

        let offset = self.buffer.len();
        write!(self.buffer, "{}", index).unwrap();
        let length = self.buffer.len() - offset;

        self.lengths.push(length as u16);
        self.offsets.push(offset as u32);

        index
    }

    #[inline(always)]
    pub fn find_index(&self, string: &str) -> Option<usize> {
        let target_length = string.len() as u16;

        // there can be gaps in the parts of the string that we use (because of updates)
        // hence we can't just sum the lengths we've seen so far to get the next offset
        for (index, length) in self.lengths.iter().enumerate() {
            if *length == target_length {
                let offset = self.offsets[index];
                let slice = &self.buffer[offset as usize..][..*length as usize];

                if string.as_bytes() == slice {
                    return Some(index);
                }
            }
        }

        None
    }

    fn get(&self, index: usize) -> &str {
        let length = self.lengths[index] as usize;
        let offset = self.offsets[index] as usize;

        let bytes = &self.buffer[offset..][..length];

        unsafe { std::str::from_utf8_unchecked(bytes) }
    }

    pub fn try_get(&self, index: usize) -> Option<&str> {
        if index < self.lengths.len() {
            Some(self.get(index))
        } else {
            None
        }
    }

    pub fn update(&mut self, index: usize, new_string: &str) {
        let length = new_string.len();
        let offset = self.buffer.len();

        // future optimization idea: if the current name bytes are at the end of
        // `buffer`, we can update them in-place
        self.buffer.extend(new_string.bytes());

        self.lengths[index] = length as u16;
        self.offsets[index] = offset as u32;
    }

    pub fn find_and_update(&mut self, old_string: &str, new_string: &str) -> Option<usize> {
        match self.find_index(old_string) {
            Some(index) => {
                self.update(index, new_string);

                Some(index)
            }
            None => None,
        }
    }

    pub fn len(&self) -> usize {
        self.lengths.len()
    }

    pub fn is_empty(&self) -> bool {
        self.lengths.is_empty()
    }
}

#[cfg(test)]
mod test {
    use super::SmallStringInterner;

    #[test]
    fn update_key() {
        let mut interner = SmallStringInterner::default();

        interner.insert("main");
        interner.insert("a");
        assert!(interner.find_and_update("a", "ab").is_some());
        interner.insert("c");
        assert!(interner.find_and_update("c", "cd").is_some());
    }
}
