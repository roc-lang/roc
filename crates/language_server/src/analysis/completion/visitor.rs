use log::trace;

use roc_can::{
    def::Def,
    expr::{ClosureData, Expr, WhenBranch},
    pattern::{ListPatterns, Pattern, RecordDestruct, TupleDestruct},
    traverse::{walk_decl, walk_def, walk_expr, DeclarationInfo, Visitor},
};

use roc_module::symbol::{Interns, Symbol};
use roc_region::all::{Loc, Position, Region};
use roc_types::subs::Variable;

pub(crate) struct CompletionVisitor<'a> {
    pub(crate) position: Position,
    pub(crate) found_declarations: Vec<(Symbol, Variable)>,
    pub(crate) interns: &'a Interns,
    pub(crate) prefix: String,
}

impl Visitor for CompletionVisitor<'_> {
    fn should_visit(&mut self, region: Region) -> bool {
        region.contains_pos(self.position)
    }

    fn visit_expr(&mut self, expr: &Expr, region: Region, var: Variable) {
        if region.contains_pos(self.position) {
            let mut res = self.expression_defs(expr);
            self.found_declarations.append(&mut res);

            walk_expr(self, expr, var);
        }
    }

    fn visit_decl(&mut self, decl: DeclarationInfo<'_>) {
        match decl {
            DeclarationInfo::Value { loc_expr, .. }
            | DeclarationInfo::Function {
                loc_body: loc_expr, ..
            }
            | DeclarationInfo::Destructure { loc_expr, .. } => {
                let res = self.decl_to_completion_item(&decl);
                self.found_declarations.extend(res);

                if loc_expr.region.contains_pos(self.position) {
                    walk_decl(self, decl);
                };
            }
            _ => {
                walk_decl(self, decl);
            }
        }
    }

    fn visit_def(&mut self, def: &Def) {
        let sym_var_vec = self.extract_defs(def);
        self.found_declarations.extend(sym_var_vec);

        walk_def(self, def);
    }
}

impl CompletionVisitor<'_> {
    fn extract_defs(&mut self, def: &Def) -> Vec<(Symbol, Variable)> {
        trace!("Completion begin");

        def.pattern_vars
            .iter()
            .map(|(symbol, var)| (*symbol, *var))
            .collect()
    }

    fn expression_defs(&self, expr: &Expr) -> Vec<(Symbol, Variable)> {
        match expr {
            Expr::When {
                expr_var, branches, ..
            } => self.when_is_expr(branches, expr_var),
            Expr::Closure(ClosureData {
                arguments,
                loc_body,
                ..
            }) => {
                // if we are inside the closure complete it's vars
                if loc_body.region.contains_pos(self.position) {
                    arguments
                        .iter()
                        .flat_map(|(var, _, pat)| self.patterns(&pat.value, var))
                        .collect()
                } else {
                    vec![]
                }
            }
            _ => vec![],
        }
    }

    /// Extract any variables made available by the branch of a when_is expression that contains `self.position`
    fn when_is_expr(
        &self,
        branches: &[WhenBranch],
        expr_var: &Variable,
    ) -> Vec<(Symbol, Variable)> {
        branches
            .iter()
            .flat_map(
                |WhenBranch {
                     patterns, value, ..
                 }| {
                    if value.region.contains_pos(self.position) {
                        patterns
                            .iter()
                            .flat_map(|pattern| self.patterns(&pattern.pattern.value, expr_var))
                            .collect()
                    } else {
                        vec![]
                    }
                },
            )
            .collect()
    }

    fn record_destructure(&self, destructs: &[Loc<RecordDestruct>]) -> Vec<(Symbol, Variable)> {
        destructs
            .iter()
            .flat_map(|loc| match &loc.value.typ {
                roc_can::pattern::DestructType::Required
                | roc_can::pattern::DestructType::Optional(_, _) => {
                    vec![(loc.value.symbol, loc.value.var)]
                }
                roc_can::pattern::DestructType::Guard(var, pat) => self.patterns(&pat.value, var),
            })
            .collect()
    }

    fn tuple_destructure(&self, destructs: &[Loc<TupleDestruct>]) -> Vec<(Symbol, Variable)> {
        destructs
            .iter()
            .flat_map(|loc| {
                let (var, pattern) = &loc.value.typ;
                self.patterns(&pattern.value, var)
            })
            .collect()
    }

    fn list_pattern(&self, list_elems: &ListPatterns, var: &Variable) -> Vec<(Symbol, Variable)> {
        list_elems
            .patterns
            .iter()
            .flat_map(|loc| self.patterns(&loc.value, var))
            .collect()
    }
    fn tag_pattern(&self, arguments: &[(Variable, Loc<Pattern>)]) -> Vec<(Symbol, Variable)> {
        arguments
            .iter()
            .flat_map(|(var, pat)| self.patterns(&pat.value, var))
            .collect()
    }

    fn as_pattern(
        &self,
        as_pat: &Pattern,
        as_symbol: Symbol,
        var: &Variable,
    ) -> Vec<(Symbol, Variable)> {
        // get the variables introduced within the pattern
        let mut patterns = self.patterns(as_pat, var);
        // add the "as" that wraps the whole pattern
        patterns.push((as_symbol, *var));
        patterns
    }

    /// Returns a list of symbols defined by this pattern.  
    /// `pattern_var`: Variable type of the entire pattern. This will be returned if
    /// the pattern turns out to be an identifier.
    fn patterns(
        &self,
        pattern: &roc_can::pattern::Pattern,
        pattern_var: &Variable,
    ) -> Vec<(Symbol, Variable)> {
        match pattern {
            roc_can::pattern::Pattern::Identifier(symbol) => {
                if self.is_match(symbol) {
                    vec![(*symbol, *pattern_var)]
                } else {
                    vec![]
                }
            }
            Pattern::AppliedTag { arguments, .. } => self.tag_pattern(arguments),
            Pattern::UnwrappedOpaque { argument, .. } => {
                self.patterns(&argument.1.value, &argument.0)
            }
            Pattern::List {
                elem_var, patterns, ..
            } => self.list_pattern(patterns, elem_var),
            roc_can::pattern::Pattern::As(pat, symbol) => {
                self.as_pattern(&pat.value, *symbol, pattern_var)
            }
            roc_can::pattern::Pattern::RecordDestructure {
                destructs,
                opt_spread,
                whole_var: _,
            } => self.record_destructure(destructs, opt_spread),
            roc_can::pattern::Pattern::TupleDestructure { destructs, .. } => {
                self.tuple_destructure(destructs)
            }
            _ => vec![],
        }
    }

    fn is_match(&self, symbol: &Symbol) -> bool {
        symbol.as_str(self.interns).starts_with(&self.prefix)
    }

    fn decl_to_completion_item(&self, decl: &DeclarationInfo) -> Vec<(Symbol, Variable)> {
        match decl {
            DeclarationInfo::Value {
                expr_var, pattern, ..
            } => self.patterns(pattern, expr_var),
            DeclarationInfo::Function {
                expr_var,
                pattern,
                function,
                loc_body,
                ..
            } => {
                let mut sym_var_vec = vec![];
                // append the function declaration itself for recursive calls
                sym_var_vec.extend(self.patterns(pattern, expr_var));

                if loc_body.region.contains_pos(self.position) {
                    // also add the arguments if we are inside the function
                    let args = function
                        .value
                        .arguments
                        .iter()
                        .flat_map(|(var, _, pat)| self.patterns(&pat.value, var));
                    // we add in the pattern for the function declaration
                    sym_var_vec.extend(args);

                    trace!(
                        "Added function args to completion output =:{:#?}",
                        sym_var_vec
                    );
                }

                sym_var_vec
            }
            DeclarationInfo::Destructure {
                loc_pattern,
                expr_var,
                ..
            } => self.patterns(&loc_pattern.value, expr_var),
            DeclarationInfo::Expectation { .. } => vec![],
        }
    }
}
