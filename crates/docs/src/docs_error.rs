use peg::error::ParseError;
use roc_ast::ast_error::ASTError;
use roc_module::module_err::ModuleError;
use roc_parse::parser::SyntaxError;
use snafu::Snafu;

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
    WrapPegParseError {
        source: ParseError<usize>,
    },
}

pub type DocsResult<T, E = DocsError> = std::result::Result<T, E>;

impl<'a> From<SyntaxError<'a>> for DocsError {
    fn from(syntax_err: SyntaxError) -> Self {
        Self::WrapSyntaxError {
            msg: format!("{:?}", syntax_err),
        }
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

impl From<ParseError<usize>> for DocsError {
    fn from(peg_parse_err: ParseError<usize>) -> Self {
        Self::WrapPegParseError {
            source: peg_parse_err,
        }
    }
}
