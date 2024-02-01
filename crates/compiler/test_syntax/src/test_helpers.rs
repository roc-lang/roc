use bumpalo::Bump;
use roc_fmt::{annotation::Formattable, module::fmt_module};
use roc_parse::{
    ast::{Defs, Expr, Malformed, Module},
    module::parse_module_defs,
    parser::{Parser, SyntaxError},
    state::State,
    test_helpers::{parse_defs_with, parse_expr_with, parse_header_with},
};
use roc_test_utils::assert_multiline_str_eq;

use roc_fmt::spaces::RemoveSpaces;
use roc_fmt::Buf;

/// Source code to parse. Usually in the form of a test case.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Input<'a> {
    /// A header (e.g. `interface "foo" ...`)
    Header(&'a str),

    /// A sequence of module definitions (e.g. `f = \x -> x + 1`)
    ModuleDefs(&'a str),

    /// A single expression
    Expr(&'a str),

    /// Both the header and the module defs
    Full(&'a str),
}

// Owned version of `Input`
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InputOwned {
    Header(String),
    ModuleDefs(String),
    Expr(String),
    Full(String),
}

impl InputOwned {
    fn as_ref(&self) -> Input {
        match self {
            InputOwned::Header(s) => Input::Header(s),
            InputOwned::ModuleDefs(s) => Input::ModuleDefs(s),
            InputOwned::Expr(s) => Input::Expr(s),
            InputOwned::Full(s) => Input::Full(s),
        }
    }
}

/// Output AST of a successful parse
#[derive(Debug, Clone)]
pub enum Output<'a> {
    Header(Module<'a>),

    ModuleDefs(Defs<'a>),

    Expr(Expr<'a>),

    Full {
        header: Module<'a>,
        module_defs: Defs<'a>,
    },
}

impl<'a> Output<'a> {
    fn format(&self) -> InputOwned {
        let arena = Bump::new();
        let mut buf = Buf::new_in(&arena);
        match self {
            Output::Header(header) => {
                fmt_module(&mut buf, header);
                buf.fmt_end_of_file();
                InputOwned::Header(buf.as_str().to_string())
            }
            Output::ModuleDefs(defs) => {
                defs.format(&mut buf, 0);
                buf.fmt_end_of_file();
                InputOwned::ModuleDefs(buf.as_str().to_string())
            }
            Output::Expr(expr) => {
                expr.format(&mut buf, 0);
                InputOwned::Expr(buf.as_str().to_string())
            }
            Output::Full {
                header,
                module_defs,
            } => {
                fmt_module(&mut buf, header);
                module_defs.format(&mut buf, 0);
                buf.fmt_end_of_file();
                InputOwned::Full(buf.as_str().to_string())
            }
        }
    }

    pub fn debug_format_inner(&self) -> String {
        match self {
            Output::Header(header) => format!("{header:#?}\n"),
            Output::ModuleDefs(defs) => format!("{defs:#?}\n"),
            Output::Expr(expr) => format!("{expr:#?}\n"),
            Output::Full { .. } => format!("{self:#?}\n"),
        }
    }
}

impl<'a> Malformed for Output<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            Output::Header(header) => header.is_malformed(),
            Output::ModuleDefs(defs) => defs.is_malformed(),
            Output::Expr(expr) => expr.is_malformed(),
            Output::Full {
                header,
                module_defs,
            } => header.is_malformed() || module_defs.is_malformed(),
        }
    }
}

impl<'a> RemoveSpaces<'a> for Output<'a> {
    fn remove_spaces(&self, arena: &'a Bump) -> Self {
        match self {
            Output::Header(header) => Output::Header(header.remove_spaces(arena)),
            Output::ModuleDefs(defs) => Output::ModuleDefs(defs.remove_spaces(arena)),
            Output::Expr(expr) => Output::Expr(expr.remove_spaces(arena)),
            Output::Full {
                header,
                module_defs,
            } => Output::Full {
                header: header.remove_spaces(arena),
                module_defs: module_defs.remove_spaces(arena),
            },
        }
    }
}

impl<'a> Input<'a> {
    pub fn as_str(&self) -> &'a str {
        match self {
            Input::Header(s) => s,
            Input::ModuleDefs(s) => s,
            Input::Expr(s) => s,
            Input::Full(s) => s,
        }
    }

    pub fn parse_in(&self, arena: &'a Bump) -> Result<Output<'a>, SyntaxError<'a>> {
        match self {
            Input::Header(input) => {
                let header = parse_header_with(arena, input)?;
                Ok(Output::Header(header))
            }

            Input::ModuleDefs(input) => {
                let module_defs = parse_defs_with(arena, input)?;
                Ok(Output::ModuleDefs(module_defs))
            }

            Input::Expr(input) => {
                let expr = parse_expr_with(arena, input)?;
                Ok(Output::Expr(expr))
            }

            Input::Full(input) => {
                let state = State::new(input.as_bytes());

                let min_indent = 0;
                let (_, header, state) = roc_parse::module::header()
                    .parse(arena, state.clone(), min_indent)
                    .map_err(|(_, fail)| SyntaxError::Header(fail))?;

                let module_defs = parse_module_defs(arena, state, Defs::default())?;

                Ok(Output::Full {
                    header,
                    module_defs,
                })
            }
        }
    }

    /// Parse and re-format the given input, and pass the output to `check_formatting`
    /// for verification.  The expectation is that `check_formatting` assert the result matches
    /// expectations (or, overwrite the expectation based on a command-line flag)
    /// Optionally, based on the value of `check_idempotency`, also verify that the formatting
    /// is idempotent - that if we reformat the output, we get the same result.
    pub fn check_invariants(
        &self,
        handle_formatted_output: impl Fn(Input),
        check_idempotency: bool,
    ) {
        let arena = Bump::new();

        let actual = self.parse_in(&arena).unwrap_or_else(|err| {
            panic!("Unexpected parse failure when parsing this for formatting:\n\n{}\n\nParse error was:\n\n{:?}\n\n", self.as_str(), err);
        });

        let output = actual.format();

        handle_formatted_output(output.as_ref());

        let reparsed_ast = output.as_ref().parse_in(&arena).unwrap_or_else(|err| {
            panic!(
                "After formatting, the source code no longer parsed!\n\n\
                Parse error was: {:?}\n\n\
                The original code was:\n\n{}\n\n\
                The code that failed to parse:\n\n{}\n\n\
                The original ast was:\n\n{:#?}\n\n",
                err,
                self.as_str(),
                output.as_ref().as_str(),
                actual
            );
        });

        let ast_normalized = actual.remove_spaces(&arena);
        let reparsed_ast_normalized = reparsed_ast.remove_spaces(&arena);

        // HACK!
        // We compare the debug format strings of the ASTs, because I'm finding in practice that _somewhere_ deep inside the ast,
        // the PartialEq implementation is returning `false` even when the Debug-formatted impl is exactly the same.
        // I don't have the patience to debug this right now, so let's leave it for another day...
        // TODO: fix PartialEq impl on ast types
        if format!("{ast_normalized:?}") != format!("{reparsed_ast_normalized:?}") {
            panic!(
                "Formatting bug; formatting didn't reparse to the same AST (after removing spaces)\n\n\
                * * * Source code before formatting:\n{}\n\n\
                * * * Source code after formatting:\n{}\n\n\
                * * * AST before formatting:\n{:#?}\n\n\
                * * * AST after formatting:\n{:#?}\n\n",
                self.as_str(),
                output.as_ref().as_str(),
                actual,
                reparsed_ast_normalized
            );
        }

        // Now verify that the resultant formatting is _idempotent_ - i.e. that it doesn't change again if re-formatted
        if check_idempotency {
            let reformatted = reparsed_ast.format();

            if output != reformatted {
                eprintln!("Formatting bug; formatting is not stable.\nOriginal code:\n{}\n\nFormatted code:\n{}\n\nAST:\n{:#?}\n\nReparsed AST:\n{:#?}\n\n",
                    self.as_str(),
                    output.as_ref().as_str(),
                    actual,
                    reparsed_ast);
                eprintln!("Reformatting the formatted code changed it again, as follows:\n\n");

                assert_multiline_str_eq!(output.as_ref().as_str(), reformatted.as_ref().as_str());
            }
        }
    }
}
