use roc_fmt::Buf;

pub struct FormattedAst<'a> {
    buf: Buf<'a>,
}

impl<'a> FormattedAst<'a> {
    pub(crate) fn new(buf: Buf<'a>) -> Self {
        Self { buf }
    }

    pub fn as_str(&self) -> &str {
        self.buf.as_str()
    }
}

impl ToString for FormattedAst<'_> {
    fn to_string(&self) -> String {
        self.buf.as_str().to_owned()
    }
}
