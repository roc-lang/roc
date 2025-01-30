pub trait Push<T> {
    fn push(&mut self, entry: T);
}

impl<T> Push<T> for Vec<T> {
    fn push(&mut self, entry: T) {
        self.push(entry);
    }
}

impl<T> Push<T> for &mut Vec<T> {
    fn push(&mut self, entry: T) {
        (*self).push(entry);
    }
}
