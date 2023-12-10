use roc_can::{
    def::Def,
    expr::{Declarations, Expr, WhenBranch},
    pattern::{ListPatterns, Pattern, RecordDestruct, TupleDestruct},
    traverse::{walk_decl, walk_def, walk_expr, DeclarationInfo, Visitor},
};
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_region::all::{Loc, Position, Region};
use roc_types::subs::{Subs, Variable};
use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind};

use crate::analysis::format_var_type;

pub struct CompletionVisitor<'a> {
    position: Position,
    found_decls: Vec<(Symbol, Variable)>,
    pub interns: &'a Interns,
    pub prefix: String,
}

impl Visitor for CompletionVisitor<'_> {
    fn should_visit(&mut self, region: Region) -> bool {
        region.contains_pos(self.position)
    }

    fn visit_expr(&mut self, expr: &Expr, region: Region, var: Variable) {
        if region.contains_pos(self.position) {
            // self.region_typ = Some((region, var));
            let mut res = self.expression_defs(expr);
            self.found_decls.append(&mut res);

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
                let mut res = self.decl_to_completion_item(&decl);
                self.found_decls.append(&mut res);
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
        let mut res = self.extract_defs(def);
        self.found_decls.append(&mut res);
        walk_def(self, def);
    }
}
impl CompletionVisitor<'_> {
    fn extract_defs(&mut self, def: &Def) -> Vec<(Symbol, Variable)> {
        eprintln!("completion begin");
        def.pattern_vars
            .iter()
            .map(|(symbol, var)| (symbol.clone(), var.clone()))
            .collect()
    }
    fn expression_defs(&self, expr: &Expr) -> Vec<(Symbol, Variable)> {
        match expr {
            Expr::When {
                expr_var, branches, ..
            } => {
                let out: Vec<_> = branches
                    .iter()
                    .flat_map(
                        |WhenBranch {
                             patterns, value, ..
                         }| {
                            if value.region.contains_pos(self.position) {
                                patterns
                                    .iter()
                                    .flat_map(|pattern| {
                                        //We use the expression var here because if the pattern is an identifier then it must have the type of the expession given to the when is block
                                        self.patterns(&pattern.pattern.value, expr_var)
                                    })
                                    .collect()
                            } else {
                                vec![]
                            }
                        },
                    )
                    .collect();
                out
            }
            _ => vec![],
        }
    }

    fn record_destructs(&self, destructs: &Vec<Loc<RecordDestruct>>) -> Vec<(Symbol, Variable)> {
        destructs
            .iter()
            //TODO:I need to destructure value.typ here
            .flat_map(|a| match &a.value.typ {
                roc_can::pattern::DestructType::Required
                | roc_can::pattern::DestructType::Optional(_, _) => {
                    vec![(a.value.symbol, a.value.var)]
                }
                roc_can::pattern::DestructType::Guard(var, pat) => self.patterns(&pat.value, &var),
            })
            .collect()
    }

    fn tuple_destructs(&self, destructs: &Vec<Loc<TupleDestruct>>) -> Vec<(Symbol, Variable)> {
        destructs
            .iter()
            //TODO:I need to destructure value.typ here
            .flat_map(|a| {
                let (var, pattern) = &a.value.typ;
                self.patterns(&pattern.value, &var)
            })
            .collect()
    }

    fn list_destructure(
        &self,
        list_elems: &ListPatterns,
        var: &Variable,
    ) -> Vec<(Symbol, Variable)> {
        list_elems
            .patterns
            .iter()
            //TODO:I need to destructure value.typ here
            .flat_map(|a| self.patterns(&a.value, var))
            .collect()
    }
    fn tag_destructure(&self, arguments: &[(Variable, Loc<Pattern>)]) -> Vec<(Symbol, Variable)> {
        arguments
            .iter()
            .flat_map(|(var, pat)| self.patterns(&pat.value, var))
            .collect()
    }

    fn as_pattern(&self, as_pat: &Pattern, as_symbol: Symbol) -> Option<(Symbol, Variable)> {
        let var = match as_pat {
            Pattern::AppliedTag { whole_var, .. } => whole_var,
            Pattern::UnwrappedOpaque { whole_var, .. } => whole_var,
            Pattern::RecordDestructure { whole_var, .. } => whole_var,
            Pattern::TupleDestructure { whole_var, .. } => whole_var,
            Pattern::List { list_var, .. } => list_var,
            _ => return None,
        };
        Some((as_symbol, var.clone()))
    }
    fn patterns(
        &self,
        pattern: &roc_can::pattern::Pattern,
        var: &Variable,
    ) -> Vec<(Symbol, Variable)> {
        match pattern {
            roc_can::pattern::Pattern::Identifier(symbol) => {
                if self.is_match(symbol) {
                    vec![(symbol.clone(), var.clone())]
                } else {
                    vec![]
                }
            }
            Pattern::AppliedTag { arguments, .. } => self.tag_destructure(arguments),
            Pattern::UnwrappedOpaque { argument, .. } => {
                self.patterns(&argument.1.value, &argument.0)
            }
            Pattern::List {
                elem_var, patterns, ..
            } => self.list_destructure(patterns, elem_var),
            roc_can::pattern::Pattern::As(pat, symbol) => self
                .as_pattern(&pat.value, symbol.clone())
                .map(|a| vec![a])
                .unwrap_or(vec![]),
            roc_can::pattern::Pattern::RecordDestructure { destructs, .. } => {
                self.record_destructs(destructs)
            }
            roc_can::pattern::Pattern::TupleDestructure { destructs, .. } => {
                self.tuple_destructs(destructs)
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
                let mut out: Vec<_> = vec![];
                //append the function declaration
                out.append(&mut self.patterns(pattern, expr_var));

                if loc_body.region.contains_pos(self.position) {
                    //also add the arguments if we are inside the function
                    let mut args: Vec<_> = function
                        .value
                        .arguments
                        .iter()
                        .flat_map(|(var, _1, pat)| self.patterns(&pat.value, var))
                        //We add in the pattern for the function declaration
                        .collect();
                    out.append(&mut args);
                }
                out
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

fn get_completions<'a>(
    position: Position,
    decls: &'a Declarations,
    prefix: String,
    interns: &Interns,
) -> Vec<(Symbol, Variable)> {
    let mut visitor = CompletionVisitor {
        position,
        found_decls: Vec::new(),
        interns,
        prefix,
    };
    visitor.visit_decls(decls);
    visitor.found_decls
}

fn make_completion_item(
    subs: &mut Subs,
    module_id: &ModuleId,
    interns: &Interns,
    str: String,
    var: Variable,
) -> CompletionItem {
    let type_str = format_var_type(var, subs, module_id, interns);
    let typ = match subs.get(var.clone()).content {
        roc_types::subs::Content::Structure(var) => match var {
            roc_types::subs::FlatType::Apply(_, _) => CompletionItemKind::FUNCTION,
            roc_types::subs::FlatType::Func(_, _, _) => CompletionItemKind::FUNCTION,
            roc_types::subs::FlatType::EmptyTagUnion
            | roc_types::subs::FlatType::TagUnion(_, _) => CompletionItemKind::ENUM,
            _ => CompletionItemKind::VARIABLE,
        },
        a => {
            eprintln!(
                "No specific completionKind for variable type :{:?} defaulting to 'Variable'",
                a
            );
            CompletionItemKind::VARIABLE
        }
    };

    CompletionItem {
        label: str,
        detail: Some(type_str),
        kind: Some(typ),

        ..Default::default()
    }
}
///Gets completion items using the visitor pattern,
///This will walk through declarations that would be accessable from the provided position adding them to a list of completion items untill all accessable declarations have been fully explored

pub fn get_completion_items(
    position: Position,
    prefix: String,
    decls: &Declarations,
    subs: &mut Subs,
    module_id: &ModuleId,
    interns: &Interns,
) -> Vec<CompletionItem> {
    let completions = get_completions(position, decls, prefix, interns);
    make_completion_items(subs, module_id, interns, completions)
}
pub fn make_completion_items(
    subs: &mut Subs,
    module_id: &ModuleId,
    interns: &Interns,
    completions: Vec<(Symbol, Variable)>,
) -> Vec<CompletionItem> {
    completions
        .into_iter()
        .map(|(symbol, var)| {
            make_completion_item(
                subs,
                module_id,
                interns,
                symbol.as_str(interns).to_string(),
                var,
            )
        })
        .collect()
}

fn make_completion_items_string(
    subs: &mut Subs,
    module_id: &ModuleId,
    interns: &Interns,
    completions: Vec<(String, Variable)>,
) -> Vec<CompletionItem> {
    completions
        .into_iter()
        .map(|(symbol, var)| make_completion_item(subs, module_id, interns, symbol, var))
        .collect()
}

///Gets completion items for a record field
///Uses
fn find_record_fields(var: Variable, subs: &mut Subs) -> Vec<(String, Variable)> {
    let content = subs.get(var);
    match content.content {
        roc_types::subs::Content::Structure(typ) => match typ {
            roc_types::subs::FlatType::Record(fields, ext) => {
                let field_types = fields.unsorted_iterator(subs, ext);
                let fields: Vec<_> = match field_types {
                    Ok(field) => field
                        .map(|a| {
                            let var = match a.1 {
                                roc_types::types::RecordField::Demanded(var)
                                | roc_types::types::RecordField::Required(var)
                                | roc_types::types::RecordField::Optional(var)
                                | roc_types::types::RecordField::RigidRequired(var)
                                | roc_types::types::RecordField::RigidOptional(var) => var,
                            };
                            (a.0.clone().into(), var)
                        })
                        .collect(),
                    Err(err) => {
                        eprintln!(
                            "WARN:Error getting record field types for completion{:?}",
                            err
                        );
                        vec![]
                    }
                };
                fields
            }
            roc_types::subs::FlatType::Tuple(elems, ext) => {
                let elems = elems.unsorted_iterator(subs, ext);
                let elems: Vec<_> = match elems {
                    Ok(elem) => elem.map(|(num, var)| (num.to_string(), var)).collect(),
                    Err(err) => {
                        eprintln!("WARN:Error getting tuple elems for completion{:?}", err);
                        vec![]
                    }
                };
                elems
            }

            _ => {
                eprintln!(
                    "WARN: Trying to get field completion for a type that is not a record   ",
                );
                vec![]
            }
        },
        roc_types::subs::Content::Error => {
            //This is caused by typechecking our partially typed variable name causing the typechecking to be confused as the type of the parent variable
            //TODO! ideally i could recover using some previous typecheck result that isn't broken
            eprintln!("ERROR: variable type of record was of type error cannot access field",);
            vec![]
        }
        a => {
            eprintln!("variable before field was unsuported type:{:?}", a);
            vec![]
        }
    }
}
///Splits a completion prefix for a field into it's components
//eg a.b.c.d->("a",["b","c"],"d")
fn get_field_completion_parts(symbol_prefix: &String) -> Option<(String, Vec<String>, String)> {
    let parts: Vec<_> = symbol_prefix.split('.').collect();
    let (variable, fields) = parts.split_first()?;

    let (field, middle) = match fields.split_last() {
        Some(a) => (a.0.to_string(), a.1.iter().map(|x| x.to_string()).collect()),

        None => ("".to_string(), vec![]),
    };
    Some((variable.to_string(), middle, field))
}
pub fn field_completion(
    position: Position,
    symbol_prefix: String,
    declarations: &Declarations,
    interns: &Interns,
    subs: &mut Subs,
    module_id: &ModuleId,
) -> Option<Vec<CompletionItem>> {
    eprintln!("getting record field completions: ");
    let (variable, middle, field) = get_field_completion_parts(&symbol_prefix)?;

    eprintln!(
        "getting record field completions: variable:{:?} field{:?} middle{:?} ",
        variable, field, middle
    );
    //get the variable from within the region
    //TODO: this is kind of just a hack. We are gettting all the completions and seeing if any match the part before the dot as a way to get the Variable type of the variable before the dot. I imagine there are much faster ways to do this
    let completion = get_completions(position, declarations, variable.to_string(), interns)
        .into_iter()
        .map(|a| (a.0.as_str(&interns).to_string(), a.1))
        .next()?;

    //We iterate through all the intermediate chunks eg var.field1.field2.field3 this iterates through fields until we get to field2, becuase it's second last
    let second_last = middle.iter().fold(completion, |state, a| {
        let fields_vars = find_record_fields(state.1, subs);
        match fields_vars.into_iter().find(|field| a == &field.0) {
            None => state,
            Some(a) => a,
        }
    });

    let field_completions: Vec<_> = find_record_fields(second_last.1, subs)
        .into_iter()
        .filter(|(str, _)| str.starts_with(&field.to_string()))
        .collect();

    let field_completions =
        make_completion_items_string(subs, module_id, interns, field_completions);
    Some(field_completions)
}
