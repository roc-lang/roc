pub trait Push<T> {
    fn push(&mut self, entry: T);
}
