use std::{io::Write, path::Prefix};

use roc_can::{
    def::Def,
    expr::{Declarations, Expr, WhenBranch},
    pattern::{ListPatterns, Pattern, RecordDestruct, TupleDestruct},
    traverse::{walk_decl, walk_def, walk_expr, DeclarationInfo, FoundDeclaration, Visitor},
};
use roc_module::symbol::{self, Interns, ModuleId, Symbol};
use roc_region::all::{Loc, Position, Region};
use roc_types::subs::{GetSubsSlice, Subs, Variable};
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
    fn visit_annotation(&mut self, _pat: &roc_can::def::Annotation) {
        let mut stderr = std::io::stderr();
        // writeln!(&mut stderr, "annotation:{:?}", _pat);
    }

    // fn visit_pattern(&mut self, pat: &Pattern, region: Region, opt_var: Option<Variable>) {
    //     if region.contains_pos(self.position) {
    //         // if let Some(var) = opt_var {
    //         //     self.region_typ = Some((region, var));
    //         // }

    //         walk_pattern(self, pat);
    //     }
    // }
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
    fn make_completion_items(&mut self, found: Vec<FoundDeclaration>) -> Vec<(Symbol, Variable)> {
        found
            .iter()
            .flat_map(|comp| match comp {
                FoundDeclaration::Decl(dec) => self.decl_to_completion_item(dec),
                FoundDeclaration::Def(def) => def
                    .pattern_vars
                    .iter()
                    .map(|(symbol, var)| (symbol.clone(), var.clone()))
                    .collect(),
            })
            .collect()
    }
    fn extract_defs(&mut self, def: &Def) -> Vec<(Symbol, Variable)> {
        let mut stderr = std::io::stderr();
        writeln!(&mut stderr, "completion begin");
        def.pattern_vars
            .iter()
            .map(|(symbol, var)| (symbol.clone(), var.clone()))
            .collect()
    }
    fn expression_defs(&self, expr: &Expr) -> Vec<(Symbol, Variable)> {
        match expr {
            // Expr::Num(_, _, _, _) => todo!(),
            // Expr::Int(_, _, _, _, _) => todo!(),
            // Expr::Float(_, _, _, _, _) => todo!(),
            // Expr::Str(_) => todo!(),
            // Expr::SingleQuote(_, _, _, _) => todo!(),
            // Expr::List { elem_var, loc_elems } => todo!(),
            // Expr::IngestedFile(_, _, _) => todo!(),
            // Expr::Var(_, _) => todo!(),
            // Expr::AbilityMember(_, _, _) => todo!(),
            Expr::When { loc_cond, cond_var, expr_var, region, branches, branches_cond_var, exhaustive } => {

                let out:Vec<_> =
                branches.iter().flat_map(|WhenBranch{ patterns, value, guard, redundant }|{
                    if value.region.contains_pos(self.position) {
                    patterns.iter().flat_map(|pattern|{
                                //We use the expression var here because if the pattern is an identifier then it must have the type of the expession given to the when is block
                    self.patterns(&pattern.pattern.value,expr_var)}).collect()
                        }
                    else{
                            vec![]}
                    }).collect();
                out
            },
            // Expr::If { cond_var, branch_var, branches, final_else } => todo!(),
            _=>vec![]
            // Expr::LetRec(_, _, _) => todo!(),
            // Expr::LetNonRec(_, _) => todo!(),
            // Expr::Call(_, _, _) => todo!(),
            // Expr::RunLowLevel { op, args, ret_var } => todo!(),
            // Expr::ForeignCall { foreign_symbol, args, ret_var } => todo!(),
            // Expr::Closure(_) => todo!(),
            // Expr::Record { record_var, fields } => todo!(),
            // Expr::EmptyRecord => todo!(),
            // Expr::Tuple { tuple_var, elems } => todo!(),
            // Expr::Crash { msg, ret_var } => todo!(),
            // Expr::RecordAccess { record_var, ext_var, field_var, loc_expr, field } => todo!(),
            // Expr::RecordAccessor(_) => todo!(),
            // Expr::TupleAccess { tuple_var, ext_var, elem_var, loc_expr, index } => todo!(),
            // Expr::RecordUpdate { record_var, ext_var, symbol, updates } => todo!(),
            // Expr::Tag { tag_union_var, ext_var, name, arguments } => todo!(),
            // Expr::ZeroArgumentTag { closure_name, variant_var, ext_var, name } => todo!(),
            // Expr::OpaqueRef { opaque_var, name, argument, specialized_def_type, type_arguments, lambda_set_variables } => todo!(),
            // Expr::OpaqueWrapFunction(_) => todo!(),
            // Expr::Expect { loc_condition, loc_continuation, lookups_in_cond } => todo!(),
            // Expr::ExpectFx { loc_condition, loc_continuation, lookups_in_cond } => todo!(),
            // Expr::Dbg { loc_message, loc_continuation, variable, symbol } => todo!(),
            // Expr::TypedHole(_) => todo!(),
            // Expr::RuntimeError(_) => todo!(),
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

    fn as_pattern(&self, as_pat: &Pattern, as_symbol: Symbol) -> (Symbol, Variable) {
        let var = match as_pat {
            Pattern::AppliedTag {
                whole_var,
                ext_var,
                tag_name,
                arguments,
            } => whole_var,
            Pattern::UnwrappedOpaque {
                whole_var,
                opaque,
                argument,
                specialized_def_type,
                type_arguments,
                lambda_set_variables,
            } => whole_var,
            Pattern::RecordDestructure {
                whole_var,
                ext_var,
                destructs,
            } => whole_var,
            Pattern::TupleDestructure {
                whole_var,
                ext_var,
                destructs,
            } => whole_var,
            Pattern::List {
                list_var,
                elem_var,
                patterns,
            } => list_var,
            // Pattern::NumLiteral(_, _, _, _) => todo!(),
            // Pattern::IntLiteral(_, _, _, _, _) => todo!(),
            // Pattern::FloatLiteral(_, _, _, _, _) => todo!(),
            // Pattern::StrLiteral(_) => todo!(),
            // Pattern::SingleQuote(_, _, _, _) => todo!(),
            // Pattern::Underscore => todo!(),
            // Pattern::AbilityMemberSpecialization { ident, specializes } => todo!(),
            // Pattern::Shadowed(_, _, _) => todo!(),
            // Pattern::OpaqueNotInScope(_) => todo!(),
            // Pattern::UnsupportedPattern(_) => todo!(),
            // Pattern::MalformedPattern(_, _) => todo!(),
            _ => todo!(),
        };
        (as_symbol, var.clone())
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
            // Pattern::AppliedTag {
            //     whole_var,
            //     ext_var,
            //     tag_name,
            //     arguments,
            // } => whole_var,
            // Pattern::UnwrappedOpaque {
            //     whole_var,
            //     opaque,
            //     argument,
            //     specialized_def_type,
            //     type_arguments,
            //     lambda_set_variables,
            // } => whole_var,
            Pattern::List {
                list_var,
                elem_var,
                patterns,
            } => self.list_destructure(patterns, elem_var),
            roc_can::pattern::Pattern::As(pat, symbol) => {
                vec![self.as_pattern(&pat.value, symbol.clone())]
            }
            roc_can::pattern::Pattern::RecordDestructure {
                whole_var,
                ext_var,
                destructs,
            } => self.record_destructs(destructs),
            roc_can::pattern::Pattern::TupleDestructure {
                whole_var,
                ext_var,
                destructs,
            } => self.tuple_destructs(destructs),
            // roc_can::pattern::Pattern::List {
            //     list_var,
            //     elem_var,
            //     patterns,
            // } => todo!(),
            // roc_can::pattern::Pattern::NumLiteral(_, _, _, _) => todo!(),
            // roc_can::pattern::Pattern::IntLiteral(_, _, _, _, _) => todo!(),
            // roc_can::pattern::Pattern::FloatLiteral(_, _, _, _, _) => todo!(),
            // roc_can::pattern::Pattern::StrLiteral(_) => todo!(),
            // roc_can::pattern::Pattern::SingleQuote(_, _, _, _) => todo!(),
            // roc_can::pattern::Pattern::Underscore => todo!(),
            // roc_can::pattern::Pattern::AbilityMemberSpecialization {
            //     ident,
            //     specializes,
            // } => todo!(),
            // roc_can::pattern::Pattern::Shadowed(_, _, _) => todo!(),
            // roc_can::pattern::Pattern::OpaqueNotInScope(_) => todo!(),
            // roc_can::pattern::Pattern::UnsupportedPattern(_) => todo!(),
            // roc_can::pattern::Pattern::MalformedPattern(_, _) => todo!(),
            _ => vec![],
        }
    }

    fn is_match(&self, symbol: &Symbol) -> bool {
        let mut stderr = std::io::stderr();
        // writeln!(
        //     &mut stderr,
        //     "check if prefix {:?} matches {:?}",
        //     self.prefix,
        //     symbol.as_str(self.interns)
        // );
        symbol.as_str(self.interns).starts_with(&self.prefix)
    }

    fn decl_to_completion_item(&self, decl: &DeclarationInfo) -> Vec<(Symbol, Variable)> {
        match decl {
            DeclarationInfo::Value {
                loc_symbol,
                expr_var,
                pattern,
                ..
            } => {
                let mut stderr = std::io::stderr();
                // writeln!(
                //     &mut stderr,
                //     "decl:{:?}",
                //     loc_symbol.value.as_str(self.interns)
                // );

                self.patterns(pattern, expr_var)
            }
            DeclarationInfo::Function {
                loc_symbol,
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

pub fn get_completions<'a>(
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
//TODO: this should be replaced with a more specific solution. I can likely use the variable type to figure out what the completion item kind is
// fn make_completion_item_var(subs:&Subs,symbol: &Symbol, var: &Variable) -> CompletionItem {
//     make_completion_item(symbol, var, CompletionItemKind::VARIABLE)
// }
fn make_completion_item(
    subs: &mut Subs,
    module_id: &ModuleId,
    interns: &Interns,
    str: String,
    var: Variable,
) -> CompletionItem {
    let type_str = format_var_type(var, subs, module_id, interns);
    let typ = match subs.get(var.clone()).content {
        // roc_types::subs::Content::FlexVar(_) => todo!(),
        // roc_types::subs::Content::RigidVar(_) => todo!(),
        // roc_types::subs::Content::FlexAbleVar(_, _) => todo!(),
        // roc_types::subs::Content::RigidAbleVar(_, _) => todo!(),
        // roc_types::subs::Content::RecursionVar { structure, opt_name } => todo!(),
        // roc_types::subs::Content::LambdaSet(_) => todo!(),
        // roc_types::subs::Content::ErasedLambda => todo!(),
        roc_types::subs::Content::Structure(var) => match var {
            roc_types::subs::FlatType::Apply(_, _) => CompletionItemKind::FUNCTION,
            roc_types::subs::FlatType::Func(_, _, _) => CompletionItemKind::FUNCTION,
            // roc_types::subs::FlatType::FunctionOrTagUnion(_, _, _) => todo!(),
            // roc_types::subs::FlatType::RecursiveTagUnion(_, _, _) => todo!(),
            // roc_types::subs::FlatType::EmptyRecord |
            // roc_types::subs::FlatType::Record(_, _) => todo!(),
            // roc_types::subs::FlatType::EmptyTuple |
            // roc_types::subs::FlatType::Tuple(_, _) => CompletionItemKind::VARIABLE,
            roc_types::subs::FlatType::EmptyTagUnion
            | roc_types::subs::FlatType::TagUnion(_, _) => CompletionItemKind::ENUM,
            _ => CompletionItemKind::VARIABLE,
        },
        // roc_types::subs::Content::Alias(_, _, _, _) => todo!(),
        // roc_types::subs::Content::RangedNumber(_) => todo!(),
        // roc_types::subs::Content::Error => todo!(),
        a => {
            writeln!(std::io::stderr(), "unhandled variable type:{:?}", a);
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

pub fn make_completion_items_string(
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

pub fn find_record_fields(var: Variable, subs: &mut Subs) -> Vec<(String, Variable)> {
    let content = subs.get(var);
    match content.content {
        // roc_types::subs::Content::FlexVar(_) => todo!(),
        // roc_types::subs::Content::RigidVar(_) => todo!(),
        // roc_types::subs::Content::FlexAbleVar(_, _) => todo!(),
        // roc_types::subs::Content::RigidAbleVar(_, _) => todo!(),
        // roc_types::subs::Content::RecursionVar { structure, opt_name } => todo!(),
        // roc_types::subs::Content::LambdaSet(_) => todo!(),
        // roc_types::subs::Content::ErasedLambda => todo!(),
        roc_types::subs::Content::Structure(typ) => match typ {
            // roc_types::subs::FlatType::Apply(_, _) => todo!(),
            // roc_types::subs::FlatType::Func(_, _, _) => todo!(),
            roc_types::subs::FlatType::Record(fields, ext) => {
                let field_types = fields.unsorted_iterator(subs, ext);
                let fields: Vec<_> = match field_types {
                    Ok(field) => field.map(|a| {
                        let var = match a.1 {
                            roc_types::types::RecordField::Demanded(var)
                            | roc_types::types::RecordField::Required(var)
                            | roc_types::types::RecordField::Optional(var)
                            | roc_types::types::RecordField::RigidRequired(var)
                            | roc_types::types::RecordField::RigidOptional(var) => var,
                        };
                        (a.0.clone().into(), var)
                    }),
                    Err(err) => todo!(),
                }
                .collect();
                fields
            }
            _ => todo!(),
            // roc_types::subs::FlatType::Tuple(_, _) => todo!(),
            // roc_types::subs::FlatType::TagUnion(_, _) => todo!(),
            // roc_types::subs::FlatType::FunctionOrTagUnion(_, _, _) => todo!(),
            // roc_types::subs::FlatType::RecursiveTagUnion(_, _, _) => todo!(),
            // roc_types::subs::FlatType::EmptyRecord => todo!(),
            // roc_types::subs::FlatType::EmptyTuple => todo!(),
            // roc_types::subs::FlatType::EmptyTagUnion => todo!(),
        },
        // roc_types::subs::Content::Alias(_, _, _, _) => todo!(),
        // roc_types::subs::Content::RangedNumber(_) => todo!(),
        roc_types::subs::Content::Error => {
            writeln!(std::io::stderr(), "ERROR: variable was of type error",);
            vec![]
        }
        a => {
            writeln!(std::io::stderr(), "variable before field type:{:?}", a);
            todo!();
        }
    }
}
pub fn field_completion(
    position: Position,
    symbol_prefix: String,
    declarations: &Declarations,
    interns: &Interns,
    subs: &mut Subs,
    module_id: &ModuleId,
) -> Option<Vec<CompletionItem>> {
    writeln!(std::io::stderr(), "getting record field completions: ");
    let mut parts: Vec<_> = symbol_prefix.split('.').collect();
    let (variable, fields) = parts.split_first_mut()?;

    let mut empty = "";
    let (field, middle) = match fields.split_last_mut() {
        Some(a) => a,

        None => {
            let out: &mut [&str] = [].as_mut_slice();
            (&mut empty, out)
        }
    };

    writeln!(
        std::io::stderr(),
        "getting record field completions: variable:{:?} field{:?} middle{:?} ",
        variable,
        field,
        middle
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
        match fields_vars
            .into_iter()
            .find(|field| a.to_string() == field.0)
        {
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
// fn make_completion_item_string(
//     &mut self,
//     label: String,
//     var: &Variable,
// ) -> CompletionItem {
//     let type_str = format_var_type(var.clone(), self.subs, self.module_id, self.interns);
//     CompletionItem {
//         label,
//         detail: Some(type_str),
//         kind: Some(CompletionItemKind::VARIABLE ),

//         ..Default::default()
//     }
// }
