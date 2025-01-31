use roc_can::{
    def::{Def, DefKind},
    expr::{Declarations, Expr},
    traverse::{self, DeclarationInfo, Visitor},
};
use roc_region::all::Region;
use roc_types::subs::Variable;
use std::ops::Range;

pub struct FoundDeclaration {
    pub var: Variable,
    pub range: Range<usize>,
}

pub enum NotFound {
    TopLevel,
    AlreadyAnnotated,
}

pub fn find_declaration_at(
    region: Region,
    decls: &Declarations,
) -> Result<FoundDeclaration, NotFound> {
    let mut visitor = Finder {
        region,
        found: Err(NotFound::TopLevel),
    };

    visitor.visit_decls(decls);
    return visitor.found;

    struct Finder {
        region: Region,
        found: Result<FoundDeclaration, NotFound>,
    }

    impl Visitor for Finder {
        fn should_visit(&mut self, region: Region) -> bool {
            region.contains(&self.region)
        }

        fn visit_decl(&mut self, decl: DeclarationInfo<'_>) {
            if self.should_visit(decl.region()) {
                match decl {
                    DeclarationInfo::Value { loc_expr, .. }
                        if matches!(loc_expr.value, Expr::ImportParams(..)) => {}
                    DeclarationInfo::Value {
                        expr_var: var,
                        loc_symbol,
                        annotation,
                        ..
                    }
                    | DeclarationInfo::Function {
                        expr_var: var,
                        loc_symbol,
                        annotation,
                        ..
                    } if annotation.is_none() => {
                        let range = loc_symbol.byte_range();
                        self.found = Ok(FoundDeclaration { var, range })
                    }
                    DeclarationInfo::Destructure {
                        expr_var: var,
                        loc_pattern,
                        annotation,
                        ..
                    } if annotation.is_none() => {
                        let range = loc_pattern.byte_range();
                        self.found = Ok(FoundDeclaration { var, range })
                    }
                    DeclarationInfo::Expectation { .. } => {}
                    _ => self.found = Err(NotFound::AlreadyAnnotated),
                }
                traverse::walk_decl(self, decl)
            }
        }

        fn visit_def(&mut self, def: &Def) {
            if self.should_visit(def.region()) {
                if !matches!(def.kind, DefKind::Stmt(..)) && def.annotation.is_none() {
                    self.found = Ok(FoundDeclaration {
                        var: def.expr_var,
                        range: def.loc_pattern.byte_range(),
                    });
                }
                traverse::walk_def(self, def)
            }
        }
    }
}
