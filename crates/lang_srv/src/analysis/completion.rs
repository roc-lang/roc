use std::io::Write;

use roc_can::{
    pattern::{Pattern, RecordDestruct},
    traverse::{DeclarationInfo, FoundDeclaration},
};
use roc_module::symbol::{self, Interns, ModuleId, Symbol};
use roc_region::all::Loc;
use roc_types::subs::{Subs, Variable};
use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind};

use crate::analysis::format_var_type;

use super::AnalyzedModule;
pub(crate) struct Completion<'a> {
    pub subs: &'a mut Subs,
    pub interns: &'a Interns,
    pub module_id: &'a ModuleId,
    pub prefix: String,
}
impl Completion<'_> {
    pub fn make_completion_items(&mut self, found: Vec<FoundDeclaration>) -> Vec<CompletionItem> {
        found
            .iter()
            .flat_map(|comp| match comp {
                FoundDeclaration::Decl(dec) => self.decl_to_completion_item(dec),
                FoundDeclaration::Def(def) => def
                    .pattern_vars
                    .iter()
                    .map(|(symbol, var)| self.make_completion_item_var(symbol, var))
                    .collect(),
            })
            .collect()
    }
    pub fn maybe_complete(&mut self, found: FoundDeclaration) -> Vec<CompletionItem> {
        let mut stderr = std::io::stderr();
        writeln!(&mut stderr, "completion begin");
        match found {
            FoundDeclaration::Decl(dec) => self.decl_to_completion_item(&dec),
            FoundDeclaration::Def(def) => def
                .pattern_vars
                .iter()
                .map(|(symbol, var)| self.make_completion_item_var(symbol, var))
                .collect(),
        }
    }
    fn record_destructs(&mut self, destructs: &Vec<Loc<RecordDestruct>>) -> Vec<CompletionItem> {
        destructs
            .iter()
            .map(|a| self.make_completion_item_var(&a.value.symbol, &a.value.var))
            .collect()
    }

    fn as_pattern(&mut self, as_pat: &Pattern, as_symbol: &Symbol) -> CompletionItem {
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
        self.make_completion_item_var(&as_symbol, &var)
    }
    fn patterns(&mut self, pattern: &roc_can::pattern::Pattern) -> Vec<CompletionItem> {
        match pattern {
            // roc_can::pattern::Pattern::Identifier(symbol) => {
            //     todo!()
            // }
            roc_can::pattern::Pattern::As(pat, symbol) => {
                vec![self.as_pattern(&pat.value, symbol)]
            }
            // roc_can::pattern::Pattern::AppliedTag {
            //     whole_var,
            //     ext_var,
            //     tag_name,
            //     arguments,
            // } => todo!(),
            // roc_can::pattern::Pattern::UnwrappedOpaque {
            //     whole_var,
            //     opaque,
            //     argument,
            //     specialized_def_type,
            //     type_arguments,
            //     lambda_set_variables,
            // } => todo!(),
            roc_can::pattern::Pattern::RecordDestructure {
                whole_var,
                ext_var,
                destructs,
            } => self.record_destructs(destructs),
            // roc_can::pattern::Pattern::TupleDestructure {
            //     whole_var,
            //     ext_var,
            //     destructs,
            // } => todo!(),
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
        writeln!(
            &mut stderr,
            "check if prefix {:?} matches {:?}",
            self.prefix,
            symbol.as_str(self.interns)
        );
        symbol.as_str(self.interns).starts_with(&self.prefix)
    }
    fn decl_to_completion_item(&mut self, decl: &DeclarationInfo) -> Vec<CompletionItem> {
        match decl {
            DeclarationInfo::Value {
                loc_symbol,
                expr_var,
                ..
            } => {
                if self.is_match(&loc_symbol.value) {
                    let mut stderr = std::io::stderr();
                    writeln!(&mut stderr, "match");

                    vec![self.make_completion_item(
                        &loc_symbol.value,
                        expr_var,
                        CompletionItemKind::VARIABLE,
                    )]
                } else {
                    let mut stderr = std::io::stderr();
                    writeln!(&mut stderr, "non_match");
                    vec![]
                }
            }
            DeclarationInfo::Function {
                loc_symbol,
                expr_var,
                pattern,
                function,
                ..
            } => {
                let mut args: Vec<_> = function
                    .value
                    .arguments
                    .iter()
                    .flat_map(|(var, _1, pat)| match pat.value {
                        Pattern::Identifier(symbol) => {
                            if self.is_match(&symbol) {
                                vec![self.make_completion_item_var(&symbol, var)]
                            } else {
                                vec![]
                            }
                        }
                        _ => self.patterns(&pat.value),
                    })
                    .collect();
                if self.is_match(&loc_symbol.value) {
                    args.push(self.make_completion_item(
                        &loc_symbol.value,
                        expr_var,
                        CompletionItemKind::FUNCTION,
                    ))
                }
                args.append(&mut self.patterns(pattern));
                args
            }
            DeclarationInfo::Destructure { loc_pattern, .. } => self.patterns(&loc_pattern.value),
            DeclarationInfo::Expectation { .. } => vec![],
        }
    }
    //TODO: this should be replaced with a more specific solution. I can likely use the variable type to figure out what the completion item kind is
    fn make_completion_item_var(&mut self, symbol: &Symbol, var: &Variable) -> CompletionItem {
        self.make_completion_item(symbol, var, CompletionItemKind::VARIABLE)
    }
    fn make_completion_item(
        &mut self,
        symbol: &Symbol,
        var: &Variable,
        kind: CompletionItemKind,
    ) -> CompletionItem {
        let type_str = format_var_type(var.clone(), self.subs, self.module_id, self.interns);
        CompletionItem {
            label: symbol.as_str(self.interns).to_string(),
            detail: Some(type_str),
            kind: Some(kind),

            ..Default::default()
        }
    }
    fn make_completion_item_string(
        &mut self,
        label: String,
        var: &Variable,
        kind: CompletionItemKind,
    ) -> CompletionItem {
        let type_str = format_var_type(var.clone(), self.subs, self.module_id, self.interns);
        CompletionItem {
            label,
            detail: Some(type_str),
            kind: Some(kind),

            ..Default::default()
        }
    }
}
