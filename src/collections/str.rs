pub struct Str(pub str);

impl<'a> Into<&'a str> for &'a Str {
    fn into(self) -> &'a str {
        &self.0
    }
}
