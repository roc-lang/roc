use roc_ast::ast_error::ASTError;
use roc_module::module_err::ModuleError;
use roc_parse::parser::SyntaxError;
use snafu::{NoneError, ResultExt, Snafu};

#[derive(Debug, Snafu)]
#[snafu(visibility(pub))]
#[allow(clippy::enum_variant_names)]
pub enum DocsError {
    WrapASTError {
        #[snafu(backtrace)]
        source: ASTError,
    },
    WrapModuleError {
        #[snafu(backtrace)]
        source: ModuleError,
    },
    WrapSyntaxError {
        msg: String,
    },
}

pub type DocsResult<T, E = DocsError> = std::result::Result<T, E>;

impl<'a> From<SyntaxError<'a>> for DocsError {
    fn from(syntax_err: SyntaxError) -> Self {
        let msg = format!("{:?}", syntax_err);

        // hack to handle MarkError derive
        let dummy_res: Result<(), NoneError> = Err(NoneError {});
        dummy_res.context(WrapSyntaxError { msg }).unwrap_err()
    }
}

impl From<ASTError> for DocsError {
    fn from(ast_err: ASTError) -> Self {
        Self::WrapASTError { source: ast_err }
    }
}

impl From<ModuleError> for DocsError {
    fn from(module_err: ModuleError) -> Self {
        Self::WrapModuleError { source: module_err }
    }
}
