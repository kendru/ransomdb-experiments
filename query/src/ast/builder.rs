pub trait Builder<T> {
    fn finish(self) -> T;
}