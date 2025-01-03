use log::{debug, info};
use roc_fmt::MigrationFlags;
use std::collections::HashMap;

use bumpalo::Bump;

use roc_module::symbol::{ModuleId, Symbol};

use roc_region::all::LineInfo;

use tower_lsp::lsp_types::{
    CompletionItem, Diagnostic, GotoDefinitionResponse, Hover, HoverContents, LanguageString,
    Location, MarkedString, Position, Range, SemanticTokens, SemanticTokensResult, TextEdit, Url,
};

use crate::{
    analysis::completion::{
        field_completion, get_completion_items, get_module_completion_items,
        get_tag_completion_items,
    },
    convert::{ToRange, ToRocPosition},
};

use super::{
    parse_ast::Ast,
    semantic_tokens::arrange_semantic_tokens,
    utils::{format_var_type, is_roc_identifier_char},
    AnalysisResult, AnalyzedModule,
};

pub(super) type ModuleIdToUrl = HashMap<ModuleId, Url>;

#[derive(Debug, Clone)]
pub struct AnalyzedDocument {
    pub doc_info: DocInfo,
    pub analysis_result: AnalysisResult,
}

#[derive(Debug, Clone)]
pub struct DocInfo {
    pub url: Url,
    pub line_info: LineInfo,
    pub source: String,
    pub version: i32,
}
impl DocInfo {
    pub fn new(url: Url, source: String, version: i32) -> Self {
        Self {
            url,
            line_info: LineInfo::new(&source),
            source,
            version,
        }
    }

    #[cfg(debug_assertions)]
    #[allow(unused)]
    fn debug_log_prefix(&self, offset: usize) {
        debug!("Prefix source: {:?}", self.source);

        let last_few = self.source.get(offset - 5..offset + 5).unwrap();

        let (before, after) = last_few.split_at(5);

        debug!(
            "Starting to get completion items at offset: {:?} content: '{:?}|{:?}'",
            offset, before, after
        );
    }

    fn whole_document_range(&self) -> Range {
        let start = Position::new(0, 0);
        let end = Position::new(self.line_info.num_lines(), 0);
        Range::new(start, end)
    }

    pub fn get_prefix_at_position(&self, position: Position) -> String {
        let position = position.to_roc_position(&self.line_info);
        let offset = position.offset as usize;
        let source = &self.source.as_bytes()[..offset];
        let symbol_len = source
            .iter()
            .rev()
            .take_while(|&a| is_roc_identifier_char(&(*a as char)))
            .count();

        if symbol_len == 0 {
            return String::from("");
        }

        let symbol = &self.source[offset - symbol_len..offset];

        String::from(symbol)
    }

    pub fn format(&self) -> Option<Vec<TextEdit>> {
        let source = &self.source;
        let arena = &Bump::new();

        let ast = Ast::parse(arena, source).ok()?;
        let flags = MigrationFlags {
            snakify: false,
            parens_and_commas: false,
        };
        let fmt = ast.fmt(flags);

        if source == fmt.as_str() {
            None
        } else {
            let range = self.whole_document_range();
            let text_edit = TextEdit::new(range, fmt.to_string().to_string());
            Some(vec![text_edit])
        }
    }

    pub fn semantic_tokens(&self) -> Option<SemanticTokensResult> {
        let source = &self.source;
        let arena = &Bump::new();

        let ast = Ast::parse(arena, source).ok()?;
        let tokens = ast.semantic_tokens();

        let data = arrange_semantic_tokens(tokens, &self.line_info);

        Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data,
        }))
    }
}

impl AnalyzedDocument {
    pub fn url(&self) -> &Url {
        &self.doc_info.url
    }

    fn line_info(&self) -> &LineInfo {
        &self.doc_info.line_info
    }

    fn module(&self) -> Option<&AnalyzedModule> {
        self.analysis_result.module.as_ref()
    }

    fn location(&self, range: Range) -> Location {
        Location {
            uri: self.doc_info.url.clone(),
            range,
        }
    }

    pub fn type_checked(&self) -> bool {
        self.analysis_result.module.is_some()
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.analysis_result.diagnostics.clone()
    }

    pub fn symbol_at(&self, position: Position) -> Option<Symbol> {
        let line_info = self.line_info();

        let position = position.to_roc_position(line_info);

        let AnalyzedModule {
            declarations,
            abilities,
            ..
        } = self.module()?;

        let found_symbol =
            roc_can::traverse::find_closest_symbol_at(position, declarations, abilities)?;

        Some(found_symbol.implementation_symbol())
    }

    pub fn hover(&self, position: Position) -> Option<Hover> {
        let line_info = self.line_info();

        let pos = position.to_roc_position(line_info);

        let AnalyzedModule {
            subs,
            declarations,
            module_id,
            interns,
            modules_info,
            ..
        } = self.module()?;

        let (region, var) = roc_can::traverse::find_closest_type_at(pos, declarations)?;

        //TODO: Can this be integrated into "find closest type"? Is it worth it?
        let docs_opt = self.symbol_at(position).and_then(|symbol| {
            modules_info
                .get_docs(module_id)?
                .get_doc_for_symbol(&symbol)
        });

        let type_str = format_var_type(var, &mut subs.clone(), module_id, interns);

        let range = region.to_range(self.line_info());

        let type_content = MarkedString::LanguageString(LanguageString {
            language: "roc".to_string(),
            value: type_str,
        });

        let content = vec![Some(type_content), docs_opt.map(MarkedString::String)]
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();

        Some(Hover {
            contents: HoverContents::Array(content),
            range: Some(range),
        })
    }

    pub fn definition(&self, symbol: Symbol) -> Option<GotoDefinitionResponse> {
        let AnalyzedModule { declarations, .. } = self.module()?;

        let found_declaration = roc_can::traverse::find_declaration(symbol, declarations)?;

        let range = found_declaration.region().to_range(self.line_info());

        Some(GotoDefinitionResponse::Scalar(self.location(range)))
    }

    pub(crate) fn module_url(&self, module_id: ModuleId) -> Option<Url> {
        self.module()?.module_id_to_url.get(&module_id).cloned()
    }

    pub fn completion_items(
        &self,
        position: Position,
        latest_doc: &DocInfo,
    ) -> Option<Vec<CompletionItem>> {
        let symbol_prefix = latest_doc.get_prefix_at_position(position);
        debug!(
            "Starting to get completion items for prefix: {:?} docVersion:{:?}",
            symbol_prefix, latest_doc.version
        );
        let len_diff = latest_doc.source.len() as i32 - self.doc_info.source.len() as i32;

        //We offset the position because we need the position to be in the correct scope in the most recently parsed version of the source. The quick and dirty method is to just remove the difference in length between the source files from the offset. This could cause issues, but is very easy
        //TODO: this is kind of a hack and should be removed once we can do some minimal parsing without full type checking
        let mut position = position.to_roc_position(&latest_doc.line_info);
        position.offset = (position.offset as i32 - len_diff - 1) as u32;
        debug!("Completion offset: {:?}", position.offset);

        let AnalyzedModule {
            module_id,
            interns,
            subs,
            declarations,
            exposed_imports,
            imports_by_module: imports,
            modules_info,
            ..
        } = self.module()?;

        let is_field_or_module_completion = symbol_prefix.contains('.');

        if is_field_or_module_completion {
            // If the second to last section is capitalised we know we are completing a
            // module inside an import of a module, e.g.: My.Module.function
            let is_module_completion = symbol_prefix
                .split('.')
                .nth_back(1) // second to last
                .map(|str| str.starts_with(|c: char| c.is_uppercase()))
                .unwrap_or(false);

            if is_module_completion {
                info!("Getting module dot completion...");
                Some(get_module_completion_items(
                    &symbol_prefix,
                    interns,
                    imports,
                    modules_info,
                    true,
                ))
            } else {
                info!("Getting record dot completion...");
                field_completion(
                    position,
                    symbol_prefix,
                    declarations,
                    interns,
                    &mut subs.clone(),
                    module_id,
                )
            }
        } else {
            let is_module_or_type_completion =
                symbol_prefix.starts_with(|c: char| c.is_uppercase());

            if is_module_or_type_completion {
                info!("Getting module completion...");
                let mut completions = get_module_completion_items(
                    &symbol_prefix,
                    interns,
                    imports,
                    modules_info,
                    true,
                );
                let tag_completions =
                    get_tag_completion_items(&symbol_prefix, module_id, modules_info);
                completions.extend(tag_completions);
                Some(completions)
            } else {
                info!("Getting variable completion...");
                let completions = get_completion_items(
                    position,
                    symbol_prefix,
                    declarations,
                    &mut subs.clone(),
                    module_id,
                    interns,
                    modules_info.get_docs(module_id),
                    exposed_imports,
                );
                Some(completions)
            }
        }
    }
}
