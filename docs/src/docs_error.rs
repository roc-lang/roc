use roc_ast::ast_error::ASTError;
use roc_module::module_err::ModuleError;
use roc_parse::parser::SyntaxError;
use snafu::{Backtrace, NoneError, ResultExt, Snafu};



#[derive(Debug, Snafu)]
#[snafu(visibility(pub))]
#[allow(clippy::enum_variant_names)]
pub enum DocsError {
    #[snafu(display("ASTError: {}", msg))]
    ASTErrorBacktrace { msg: String, backtrace: Backtrace },
    #[snafu(display("ModuleError: {}", msg))]
    ModuleErrorBacktrace { msg: String, backtrace: Backtrace },
    #[snafu(display("SyntaxError: {}", msg))]
    SyntaxErrorBacktrace { msg: String, backtrace: Backtrace },
}

pub type DocsResult<T, E = DocsError> = std::result::Result<T, E>;

impl<'a> From<SyntaxError<'a>> for DocsError {
    fn from(syntax_err: SyntaxError) -> Self {
        let msg = format!("{:?}", syntax_err);

        // hack to handle MarkError derive
        let dummy_res: Result<(), NoneError> = Err(NoneError {});
        dummy_res.context(SyntaxErrorBacktrace { msg }).unwrap_err()
    }
}

impl From<ASTError> for DocsError {
    fn from(ast_err: ASTError) -> Self {
        let msg = format!("{}", ast_err);

        // hack to handle MarkError derive
        let dummy_res: Result<(), NoneError> = Err(NoneError {});
        dummy_res.context(ASTErrorBacktrace { msg }).unwrap_err()
    }
}

impl From<ModuleError> for DocsError {
    fn from(ast_err: ModuleError) -> Self {
        let msg = format!("{}", ast_err);

        // hack to handle MarkError derive
        let dummy_res: Result<(), NoneError> = Err(NoneError {});
        dummy_res.context(ModuleErrorBacktrace { msg }).unwrap_err()
    }
}
