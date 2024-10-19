pub struct ParseExpr {
    arena: Box<Bump>,
    ast: Box<Ast>,
}

impl ParseExpr {
    pub fn parse(&str) -> Self {
        let mut arena = Bump::new();
        let ast = parse(arena, without_indent(str));

        Self {
            arena,
            ast,
        }
    }

    pub fn ast(&self) -> &Ast {
        self.ast
    }
}
