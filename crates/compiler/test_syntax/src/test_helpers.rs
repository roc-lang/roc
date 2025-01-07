use std::path::Path;

use bumpalo::Bump;
use roc_can::desugar;
use roc_can::env::Env;
use roc_can::expr::canonicalize_expr;
use roc_can::scope::Scope;
use roc_error_macros::set_panic_not_exit;
use roc_fmt::{annotation::Formattable, header::fmt_header, MigrationFlags};
use roc_module::ident::QualifiedModuleName;
use roc_module::symbol::{IdentIds, Interns, ModuleIds, PackageModuleIds, Symbol};
use roc_parse::ast::ValueDef;
use roc_parse::ast::{Pattern, RecursiveValueDefIter};
use roc_parse::header::parse_module_defs;
use roc_parse::parser::Parser;
use roc_parse::parser::SyntaxError;
use roc_parse::state::State;
use roc_parse::test_helpers::{parse_loc_with, parse_pattern_with};
use roc_parse::{ast::Malformed, normalize::Normalize};
use roc_parse::{
    ast::{Defs, Expr, FullAst, Header, SpacesBefore},
    test_helpers::{parse_defs_with, parse_header_with},
};
use roc_region::all::Loc;
use roc_region::all::Region;
use roc_test_utils::{assert_multiline_str_eq, pretty_compare_string};
use roc_types::{
    subs::{VarStore, Variable},
    types::{AliasVar, Type},
};

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

    /// A single pattern
    Pattern(&'a str),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum InputKind {
    Header,
    ModuleDefs,
    Expr,
    Full,
    Pattern,
}

impl InputKind {
    pub fn with_text(self, text: &str) -> Input {
        match self {
            InputKind::Header => Input::Header(text),
            InputKind::ModuleDefs => Input::ModuleDefs(text),
            InputKind::Expr => Input::Expr(text),
            InputKind::Full => Input::Full(text),
            InputKind::Pattern => Input::Pattern(text),
        }
    }
}

// Owned version of `Input`
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InputOwned {
    Header(String),
    ModuleDefs(String),
    Expr(String),
    Full(String),
    Pattern(String),
}

impl InputOwned {
    pub fn as_ref(&self) -> Input {
        match self {
            InputOwned::Header(s) => Input::Header(s),
            InputOwned::ModuleDefs(s) => Input::ModuleDefs(s),
            InputOwned::Expr(s) => Input::Expr(s),
            InputOwned::Full(s) => Input::Full(s),
            InputOwned::Pattern(s) => Input::Pattern(s),
        }
    }
}

/// Output AST of a successful parse
#[derive(Debug, Clone)]
pub enum Output<'a> {
    Header(SpacesBefore<'a, Header<'a>>),

    ModuleDefs(Defs<'a>),

    Expr(Loc<Expr<'a>>),

    Full(FullAst<'a>),

    Pattern(Loc<Pattern<'a>>),
}

impl<'a> Output<'a> {
    pub fn format(&self, flags: MigrationFlags) -> InputOwned {
        let arena = Bump::new();
        let mut buf = Buf::new_in(&arena, flags);
        match self {
            Output::Header(header) => {
                fmt_header(&mut buf, header);
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
            Output::Full(full) => {
                fmt_header(&mut buf, &full.header);
                full.defs.format(&mut buf, 0);
                buf.fmt_end_of_file();
                InputOwned::Full(buf.as_str().to_string())
            }
            Output::Pattern(patt) => {
                patt.format(&mut buf, 0);
                InputOwned::Pattern(buf.as_str().to_string())
            }
        }
    }

    pub fn debug_format_inner(&self) -> String {
        match self {
            Output::Header(header) => format!("{header:#?}\n"),
            Output::ModuleDefs(defs) => format!("{defs:#?}\n"),
            Output::Expr(expr) => format!("{expr:#?}\n"),
            Output::Full { .. } => format!("{self:#?}\n"),
            Output::Pattern(patt) => format!("{patt:#?}\n"),
        }
    }

    pub fn canonicalize(&self, arena: &Bump, src: &str) {
        set_panic_not_exit(true); // can has a bunch of internal_error! calls

        match self {
            Output::Header(_) => {}
            Output::ModuleDefs(_) => {
                // TODO: canonicalize module defs
            }
            Output::Full(_) => {
                // TODO: canonicalize full ast
            }
            Output::Pattern(_) => {}
            Output::Expr(loc_expr) => {
                let mut var_store = VarStore::default();
                let mut imported: Vec<(QualifiedModuleName, Region)> = vec![];

                let empty_defs = Defs::default();
                let mut it = RecursiveValueDefIter::new(&empty_defs);
                it.push_pending_from_expr(&loc_expr.value);
                for (def, region) in it {
                    if let ValueDef::ModuleImport(import) = def {
                        imported.push((import.name.value.into(), *region));
                    }
                }

                let mut module_ids = ModuleIds::default();
                let mut qualified_module_ids = PackageModuleIds::default();
                let mut dep_idents = IdentIds::exposed_builtins(0);

                // Make sure the module_ids has ModuleIds for all our deps,
                // then record those ModuleIds in can_module_ids for later.
                // For each of our imports, add an entry to deps_by_name
                //
                // e.g. for `import pf.Foo exposing [bar]`, add `Foo` to deps_by_name
                //
                // Also build a list of imported_values_to_expose (like `bar` above.)
                for (qualified_module_name, _region) in imported.into_iter() {
                    let pq_module_name = qualified_module_name.into_pq_module_name(None);
                    let module_id = qualified_module_ids.get_or_insert(&pq_module_name);
                    dep_idents.get_or_insert(module_id);
                }

                let home = module_ids.get_or_insert(&"Test".into());

                let mut scope = Scope::new(
                    home,
                    "TestPath".into(),
                    IdentIds::default(),
                    Default::default(),
                );

                let mut env = Env::new(
                    arena,
                    src,
                    home,
                    Path::new("Test.roc"),
                    &dep_idents,
                    &qualified_module_ids,
                    None,
                    roc_can::env::FxMode::PurityInference,
                );

                // Desugar operators (convert them to Apply calls, taking into account
                // operator precedence and associativity rules), before doing other canonicalization.
                //
                // If we did this *during* canonicalization, then each time we
                // visited a BinOp node we'd recursively try to apply this to each of its nested
                // operators, and then again on *their* nested operators, ultimately applying the
                // rules multiple times unnecessarily.
                let loc_expr = desugar::desugar_expr(&mut env, &mut scope, loc_expr);

                scope.add_alias(
                    Symbol::NUM_INT,
                    Region::zero(),
                    vec![Loc::at_zero(AliasVar::unbound(
                        "a".into(),
                        Variable::EMPTY_RECORD,
                    ))],
                    vec![],
                    Type::EmptyRec,
                    roc_types::types::AliasKind::Structural,
                );

                let (_loc_expr, _output) = canonicalize_expr(
                    &mut env,
                    &mut var_store,
                    &mut scope,
                    Region::zero(),
                    &loc_expr.value,
                );

                let mut all_ident_ids = IdentIds::exposed_builtins(1);
                all_ident_ids.insert(home, scope.locals.ident_ids);

                let _interns = Interns {
                    module_ids: env.qualified_module_ids.clone().into_module_ids(),
                    all_ident_ids,
                };
            }
        }
    }
}

impl<'a> Malformed for Output<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            Output::Header(header) => header.is_malformed(),
            Output::ModuleDefs(defs) => defs.is_malformed(),
            Output::Expr(expr) => expr.is_malformed(),
            Output::Full(full) => full.is_malformed(),
            Output::Pattern(patt) => patt.is_malformed(),
        }
    }
}

impl<'a> Normalize<'a> for Output<'a> {
    fn normalize(&self, arena: &'a Bump) -> Self {
        match self {
            Output::Header(header) => Output::Header(header.normalize(arena)),
            Output::ModuleDefs(defs) => Output::ModuleDefs(defs.normalize(arena)),
            Output::Expr(expr) => Output::Expr(expr.normalize(arena)),
            Output::Full(full) => Output::Full(full.normalize(arena)),
            Output::Pattern(patt) => Output::Pattern(patt.normalize(arena)),
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
            Input::Pattern(s) => s,
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
                let expr = parse_loc_with(arena, input).map_err(|e| e.problem)?;
                Ok(Output::Expr(expr))
            }

            Input::Full(input) => {
                let state = State::new(input.as_bytes());

                let min_indent = 0;
                let (_, header, state) = roc_parse::header::header()
                    .parse(arena, state.clone(), min_indent)
                    .map_err(|(_, fail)| SyntaxError::Header(fail))?;

                let (new_header, defs) = header.item.upgrade_header_imports(arena);
                let header = SpacesBefore {
                    before: header.before,
                    item: new_header,
                };

                let defs = parse_module_defs(arena, state, defs)?;

                Ok(Output::Full(FullAst { header, defs }))
            }

            Input::Pattern(input) => {
                let patt = parse_pattern_with(arena, input).map_err(|e| e.problem)?;
                Ok(Output::Pattern(patt))
            }
        }
    }

    pub fn check_invariants(
        &self,
        handle_formatted_output: impl Fn(Input),
        check_idempotency: bool,
        canonicalize_mode: Option<bool>,
    ) {
        self.check_invariants_with_flags(
            handle_formatted_output,
            check_idempotency,
            canonicalize_mode,
            MigrationFlags {
                snakify: false,
                parens_and_commas: false,
            },
        );
    }
    /// Parse and re-format the given input, and pass the output to `check_formatting`
    /// for verification.  The expectation is that `check_formatting` assert the result matches
    /// expectations (or, overwrite the expectation based on a command-line flag)
    /// Optionally, based on the value of `check_idempotency`, also verify that the formatting
    /// is idempotent - that if we reformat the output, we get the same result.
    pub fn check_invariants_with_flags(
        &self,
        handle_formatted_output: impl Fn(Input),
        check_idempotency: bool,
        canonicalize_mode: Option<bool>,
        flags: MigrationFlags,
    ) {
        let arena = Bump::new();

        let actual = self.parse_in(&arena).unwrap_or_else(|err| {
            panic!("Unexpected parse failure when parsing this for formatting:\n\n{}\n\nParse error was:\n\n{:#?}\n\n", self.as_str(), err);
        });

        let output = actual.format(flags);

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

        if !flags.at_least_one_active() {
            let ast_normalized = actual.normalize(&arena);
            let reparsed_ast_normalized = reparsed_ast.normalize(&arena);

            // HACK!
            // We compare the debug format strings of the ASTs, because I'm finding in practice that _somewhere_ deep inside the ast,
            // the PartialEq implementation is returning `false` even when the Debug-formatted impl is exactly the same.
            // I don't have the patience to debug this right now, so let's leave it for another day...
            // TODO: fix PartialEq impl on ast types
            if format!("{ast_normalized:?}") != format!("{reparsed_ast_normalized:?}") {
                pretty_compare_string(
                    format!("{ast_normalized:#?}").as_str(),
                    format!("{reparsed_ast_normalized:#?}").as_str(),
                );
                panic!(
                    "Formatting bug; formatting didn't reparse to the same AST (after removing spaces)\n\n\
                    * * * Source code before formatting:\n{}\n\n\
                    * * * Source code after formatting:\n{}\n\n\
                    * * * AST before formatting:\n{:#?}\n\n\
                    * * * AST after formatting:\n{:#?}\n\n",
                    self.as_str(),
                    output.as_ref().as_str(),
                    actual,
                    reparsed_ast
                );
            }
        }

        // Now verify that the resultant formatting is _idempotent_ - i.e. that it doesn't change again if re-formatted
        if check_idempotency {
            let reformatted = reparsed_ast.format(flags);

            if output != reformatted {
                pretty_compare_string(
                    format!("{actual:#?}").as_str(),
                    format!("{reparsed_ast:#?}").as_str(),
                );
                eprintln!("Formatting bug; formatting is not stable.\nOriginal code:\n{}\n\nFormatted code:\n{}\n\nAST:\n{:#?}\n\nReparsed AST:\n{:#?}\n\n",
                    self.as_str(),
                    output.as_ref().as_str(),
                    actual,
                    reparsed_ast);
                eprintln!("Reformatting the formatted code changed it again, as follows:\n\n");

                assert_multiline_str_eq!(output.as_ref().as_str(), reformatted.as_ref().as_str());
            }
        }

        if let Some(expect_panic) = canonicalize_mode {
            if expect_panic {
                let text = self.as_str();
                let res = std::panic::catch_unwind(|| {
                    let new_arena = Bump::new();
                    actual.canonicalize(&new_arena, text);
                });

                assert!(
                    res.is_err(),
                    "Canonicalize was expected to panic, but it did not. \
                    If you're running test_snapshots, you may need to remove this test from \
                    the list of tests that are expected to panic (great!), \
                    in `fn expect_canonicalize_panics`"
                );
            } else {
                // TODO grab the output here and assert things about it.
                // For now we just make sure that it doesn't crash on this input
                actual.canonicalize(&arena, self.as_str());
            }
        }
    }
}
