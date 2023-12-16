use bumpalo::collections::Vec;

pub struct IR<'a> {
    nodes: Vec<'a, Node<'a>>
}

pub enum Node<'a> {
    StrLiteral(&'a str)
}
