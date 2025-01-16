use roc_can::{
    def::{Def, DefKind},
    expr::{ClosureData, Declarations, Expr},
    traverse::{self, DeclarationInfo, Visitor},
};
use roc_error_macros::internal_error;
use roc_types::subs::Variable;

use roc_module::called_via::CalledVia;

use roc_region::all::Region;

pub struct FoundDeclaration {
    pub var: Variable,
    pub range: std::ops::Range<usize>,
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

        fn visit_expr(&mut self, expr: &Expr, region: Region, var: Variable) {
            if self.should_visit(region) {
                if let Expr::Call(_, args, CalledVia::QuestionSuffix) = expr {
                    let Expr::Closure(ClosureData { arguments, .. }) = &args[1].1.value else {
                        internal_error!("Suffixed expression did not contain a closure")
                    };
                    let loc_pattern = &arguments[0].2;
                    let expr_region = args[0].1.region;
                    let var = arguments[0].0;

                    let inner_def = matches!(args[0].1.value, Expr::LetNonRec(..));
                    let def_region = Region::span_across(&loc_pattern.region, &expr_region);

                    if !inner_def && self.should_visit(def_region) {
                        let range = loc_pattern.byte_range();
                        self.found = Ok(FoundDeclaration { var, range });
                    }
                }
                traverse::walk_expr(self, expr, var)
            }
        }
    }
}
