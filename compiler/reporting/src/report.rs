use roc_collections::all::MutSet;
use roc_module::ident::Ident;
use roc_module::ident::{Lowercase, TagName, Uppercase};
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_problem::can::PrecedenceProblem::BothNonAssociative;
use roc_problem::can::{Problem, RuntimeError};
use std::fmt;
use std::path::PathBuf;
use ven_pretty::{BoxAllocator, DocAllocator, DocBuilder, Render, RenderAnnotated};

// const IS_WINDOWS: bool = std::env::consts::OS == "windows";
const IS_WINDOWS: bool = false;

// trick to branch in a const. Can be replaced by an if when that is merged into rustc
const CYCLE_TOP: &str = ["+-----+", "┌─────┐"][(!IS_WINDOWS) as usize];
const CYCLE_LN: &str = ["|     ", "│     "][!IS_WINDOWS as usize];
const CYCLE_MID: &str = ["|     |", "│     ↓"][!IS_WINDOWS as usize];
const CYCLE_END: &str = ["+-<---+", "└─────┘"][!IS_WINDOWS as usize];

const GUTTER_BAR: &str = " ┆";

pub fn cycle<'b>(
    alloc: &'b RocDocAllocator<'b>,
    indent: usize,
    name: RocDocBuilder<'b>,
    names: Vec<RocDocBuilder<'b>>,
) -> RocDocBuilder<'b> {
    let mut lines = Vec::with_capacity(4 + (2 * names.len() - 1));

    lines.push(alloc.text(CYCLE_TOP));

    lines.push(alloc.text(CYCLE_LN).append(name));
    lines.push(alloc.text(CYCLE_MID));

    let mut it = names.into_iter().peekable();

    while let Some(other_name) = it.next() {
        lines.push(alloc.text(CYCLE_LN).append(other_name));

        if it.peek().is_some() {
            lines.push(alloc.text(CYCLE_MID));
        }
    }

    lines.push(alloc.text(CYCLE_END));

    alloc
        .vcat(lines)
        .indent(indent)
        .annotate(Annotation::TypeBlock)
}

/// A textual report.
pub struct Report<'b> {
    pub title: String,
    pub filename: PathBuf,
    pub doc: RocDocBuilder<'b>,
}

impl<'b> Report<'b> {
    /// Render to CI console output, where no colors are available.
    pub fn render_ci(self, buf: &'b mut String, alloc: &'b RocDocAllocator<'b>) {
        let err_msg = "<buffer is not a utf-8 encoded string>";

        self.pretty(&alloc)
            .1
            .render_raw(70, &mut CiWrite::new(buf))
            .expect(err_msg);
    }

    /// Render to a color terminal using ANSI escape sequences
    pub fn render_color_terminal(
        self,
        buf: &mut String,
        alloc: &'b RocDocAllocator<'b>,
        palette: &'b Palette,
    ) {
        let err_msg = "<buffer is not a utf-8 encoded string>";

        self.pretty(&alloc)
            .1
            .render_raw(70, &mut ColorWrite::new(palette, buf))
            .expect(err_msg);
    }

    pub fn pretty(self, alloc: &'b RocDocAllocator<'b>) -> RocDocBuilder<'b> {
        if self.title.is_empty() {
            self.doc
        } else {
            let header = format!(
                "-- {} {}",
                self.title,
                "-".repeat(80 - (self.title.len() + 4))
            );

            alloc.stack(vec![alloc.text(header), self.doc])
        }
    }
}

pub struct Palette<'a> {
    pub primary: &'a str,
    pub code_block: &'a str,
    pub variable: &'a str,
    pub type_variable: &'a str,
    pub structure: &'a str,
    pub alias: &'a str,
    pub error: &'a str,
    pub line_number: &'a str,
    pub gutter_bar: &'a str,
    pub module_name: &'a str,
    pub binop: &'a str,
    pub typo: &'a str,
    pub typo_suggestion: &'a str,
}

pub const DEFAULT_PALETTE: Palette = Palette {
    primary: WHITE_CODE,
    code_block: WHITE_CODE,
    variable: BLUE_CODE,
    type_variable: YELLOW_CODE,
    structure: GREEN_CODE,
    alias: YELLOW_CODE,
    error: RED_CODE,
    line_number: CYAN_CODE,
    gutter_bar: MAGENTA_CODE,
    module_name: GREEN_CODE,
    binop: GREEN_CODE,
    typo: YELLOW_CODE,
    typo_suggestion: GREEN_CODE,
};

pub const RED_CODE: &str = "\u{001b}[31m";
pub const WHITE_CODE: &str = "\u{001b}[37m";
pub const BLUE_CODE: &str = "\u{001b}[34m";
pub const YELLOW_CODE: &str = "\u{001b}[33m";
pub const GREEN_CODE: &str = "\u{001b}[42m";
pub const CYAN_CODE: &str = "\u{001b}[36m";
pub const MAGENTA_CODE: &str = "\u{001b}[35m";

pub const BOLD_CODE: &str = "\u{001b}[1m";

pub const UNDERLINE_CODE: &str = "\u{001b}[4m";

pub const RESET_CODE: &str = "\u{001b}[0m";

pub fn mono_problem<'b>(
    alloc: &'b RocDocAllocator<'b>,
    filename: PathBuf,
    problem: roc_mono::expr::MonoProblem,
) -> Report<'b> {
    use roc_mono::expr::MonoProblem::*;
    use roc_mono::pattern::Context::*;
    use roc_mono::pattern::Error::*;

    match problem {
        PatternProblem(Incomplete(region, context, missing)) => match context {
            BadArg => {
                let doc = alloc.stack(vec![
                    alloc.reflow("This pattern does not cover all the possibilities:"),
                    alloc.region(region),
                    alloc.reflow("Other possibilities include:"),
                    unhandled_patterns_to_doc_block(alloc, missing),
                    alloc.concat(vec![
                        alloc.reflow(
                            "I would have to crash if I saw one of those! \
                        So rather than pattern matching in function arguments, put a ",
                        ),
                        alloc.keyword("when"),
                        alloc.reflow(" in the function body to account for all possibilities."),
                    ]),
                ]);

                Report {
                    filename,
                    title: "UNSAFE PATTERN".to_string(),
                    doc,
                }
            }
            BadDestruct => {
                let doc = alloc.stack(vec![
                    alloc.reflow("This pattern does not cover all the possibilities:"),
                    alloc.region(region),
                    alloc.reflow("Other possibilities include:"),
                    unhandled_patterns_to_doc_block(alloc, missing),
                    alloc.concat(vec![
                        alloc.reflow(
                            "I would have to crash if I saw one of those! \
                       You can use a binding to deconstruct a value if there is only ONE possibility. \
                       Use a "
                        ),
                        alloc.keyword("when"),
                        alloc.reflow(" to account for all possibilities."),
                    ]),
                ]);

                Report {
                    filename,
                    title: "UNSAFE PATTERN".to_string(),
                    doc,
                }
            }
            BadCase => {
                let doc = alloc.stack(vec![
                    alloc.concat(vec![
                        alloc.reflow("This "),
                        alloc.keyword("when"),
                        alloc.reflow(" does not cover all the possibilities:"),
                    ]),
                    alloc.region(region),
                    alloc.reflow("Other possibilities include:"),
                    unhandled_patterns_to_doc_block(alloc, missing),
                    alloc.reflow(
                        "I would have to crash if I saw one of those! \
                        Add branches for them!",
                    ),
                    // alloc.hint().append(alloc.reflow("or use a hole.")),
                ]);

                Report {
                    filename,
                    title: "UNSAFE PATTERN".to_string(),
                    doc,
                }
            }
        },
        PatternProblem(Redundant {
            overall_region,
            branch_region,
            index,
        }) => {
            let doc = alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("The "),
                    alloc.string(index.ordinal()),
                    alloc.reflow(" pattern is redundant:"),
                ]),
                alloc.region_with_subregion(overall_region, branch_region),
                alloc.reflow(
                    "Any value of this shape will be handled by \
                a previous pattern, so this one should be removed.",
                ),
            ]);

            Report {
                filename,
                title: "REDUNDANT PATTERN".to_string(),
                doc,
            }
        }
    }
}

pub fn unhandled_patterns_to_doc_block<'b>(
    alloc: &'b RocDocAllocator<'b>,
    patterns: Vec<roc_mono::pattern::Pattern>,
) -> RocDocBuilder<'b> {
    alloc
        .vcat(patterns.into_iter().map(|v| pattern_to_doc(alloc, v)))
        .indent(4)
        .annotate(Annotation::TypeBlock)
}

fn pattern_to_doc<'b>(
    alloc: &'b RocDocAllocator<'b>,
    pattern: roc_mono::pattern::Pattern,
) -> RocDocBuilder<'b> {
    use roc_mono::pattern::Literal::*;
    use roc_mono::pattern::Pattern::*;
    //    Anything,
    //    Literal(Literal),
    //    Ctor(Union, TagName, std::vec::Vec<Pattern>),
    match pattern {
        Anything => alloc.text("_"),
        Literal(l) => match l {
            Int(i) => alloc.text(i.to_string()),
            Bit(true) => alloc.text("True"),
            Bit(false) => alloc.text("False"),
            Byte(b) => alloc.text(b.to_string()),
            Float(f) => alloc.text(f.to_string()),
            Str(s) => alloc.string(s.into()),
        },
        Ctor(_, tag_name, args) => {
            let arg_docs = args.into_iter().map(|v| pattern_to_doc(alloc, v));

            let docs = std::iter::once(alloc.tag_name(tag_name)).chain(arg_docs);

            alloc.intersperse(docs, alloc.space())
        }
    }
}

pub fn can_problem<'b>(
    alloc: &'b RocDocAllocator<'b>,
    filename: PathBuf,
    problem: Problem,
) -> Report<'b> {
    let doc = match problem {
        Problem::UnusedDef(symbol, region) => {
            let line =
                r#" then remove it so future readers of your code don't wonder why it is there."#;

            alloc.stack(vec![
                alloc
                    .symbol_unqualified(symbol)
                    .append(alloc.reflow(" is not used anywhere in your code.")),
                alloc.region(region),
                alloc
                    .reflow("If you didn't intend on using ")
                    .append(alloc.symbol_unqualified(symbol))
                    .append(alloc.reflow(line)),
            ])
        }
        Problem::UnusedImport(module_id, region) => alloc.concat(vec![
            alloc.reflow("Nothing from "),
            alloc.module(module_id),
            alloc.reflow(" is used in this module."),
            alloc.region(region),
            alloc.reflow("Since "),
            alloc.module(module_id),
            alloc.reflow(" isn't used, you don't need to import it."),
        ]),
        Problem::UnusedArgument(closure_symbol, argument_symbol, region) => {
            let line = "\". Adding an underscore at the start of a variable name is a way of saying that the variable is not used.";

            alloc.concat(vec![
                alloc.symbol_unqualified(closure_symbol),
                alloc.reflow(" doesn't use "),
                alloc.symbol_unqualified(argument_symbol),
                alloc.reflow("."),
                alloc.region(region),
                alloc.reflow("If you don't need "),
                alloc.symbol_unqualified(argument_symbol),
                alloc.reflow(", then you can just remove it. However, if you really do need "),
                alloc.symbol_unqualified(argument_symbol),
                alloc.reflow(" as an argument of "),
                alloc.symbol_unqualified(closure_symbol),
                alloc.reflow(", prefix it with an underscore, like this: \"_"),
                alloc.symbol_unqualified(argument_symbol),
                alloc.reflow(line),
            ])
        }
        Problem::PrecedenceProblem(BothNonAssociative(region, left_bin_op, right_bin_op)) => alloc
            .stack(vec![
                if left_bin_op.value == right_bin_op.value {
                    alloc.concat(vec![
                        alloc.reflow("Using more than one "),
                        alloc.binop(left_bin_op.value),
                        alloc.reflow(concat!(
                            " like this requires parentheses,",
                            " to clarify how things should be grouped.",
                        )),
                    ])
                } else {
                    alloc.concat(vec![
                        alloc.reflow("Using "),
                        alloc.binop(left_bin_op.value),
                        alloc.reflow(" and "),
                        alloc.binop(right_bin_op.value),
                        alloc.reflow(concat!(
                            " together requires parentheses, ",
                            "to clarify how they should be grouped."
                        )),
                    ])
                },
                alloc.region(region),
            ]),
        Problem::UnsupportedPattern(pattern_type, region) => {
            use roc_parse::pattern::PatternType::*;

            let this_thing = match pattern_type {
                TopLevelDef => "a top-level definition:",
                DefExpr => "a value definition:",
                FunctionArg => "function arguments:",
                WhenBranch => unreachable!("all patterns are allowed in a When"),
            };

            let suggestion = vec![
                alloc.reflow(
                    "Patterns like this don't cover all possible shapes of the input type. Use a ",
                ),
                alloc.keyword("when"),
                alloc.reflow(" ... "),
                alloc.keyword("is"),
                alloc.reflow(" instead."),
            ];

            alloc.stack(vec![
                alloc
                    .reflow("This pattern is not allowed in ")
                    .append(alloc.reflow(this_thing)),
                alloc.region(region),
                alloc.concat(suggestion),
            ])
        }
        Problem::ShadowingInAnnotation {
            original_region,
            shadow,
        } => pretty_runtime_error(
            alloc,
            RuntimeError::Shadowing {
                original_region,
                shadow,
            },
        ),
        Problem::CyclicAlias(symbol, region, others) => {
            let (doc, title) = crate::type_error::cyclic_alias(alloc, symbol, region, others);

            return Report {
                filename,
                title,
                doc,
            };
        }
        Problem::PhantomTypeArgument {
            alias,
            variable_region,
            variable_name,
        } => alloc.stack(vec![
            alloc.concat(vec![
                alloc.reflow("The "),
                alloc.type_variable(variable_name),
                alloc.reflow(" type variable is not used in the "),
                alloc.symbol_unqualified(alias),
                alloc.reflow(" alias definition:"),
            ]),
            alloc.region(variable_region),
            alloc.reflow("Roc does not allow phantom type parameters!"),
        ]),
        Problem::DuplicateRecordFieldValue {
            field_name,
            field_region,
            record_region,
            replaced_region,
        } => alloc.stack(vec![
            alloc.concat(vec![
                alloc.reflow("This record defines the "),
                alloc.record_field(field_name.clone()),
                alloc.reflow(" field twice!"),
            ]),
            alloc.region_all_the_things(
                record_region,
                replaced_region,
                field_region,
                Annotation::Error,
            ),
            alloc.reflow("In the rest of the program, I will only use the latter definition:"),
            alloc.region_all_the_things(
                record_region,
                field_region,
                field_region,
                Annotation::TypoSuggestion,
            ),
            alloc.concat(vec![
                alloc.reflow("For clarity, remove the previous "),
                alloc.record_field(field_name),
                alloc.reflow(" definitions from this record."),
            ]),
        ]),
        Problem::DuplicateRecordFieldType {
            field_name,
            field_region,
            record_region,
            replaced_region,
        } => alloc.stack(vec![
            alloc.concat(vec![
                alloc.reflow("This record type defines the "),
                alloc.record_field(field_name.clone()),
                alloc.reflow(" field twice!"),
            ]),
            alloc.region_all_the_things(
                record_region,
                replaced_region,
                field_region,
                Annotation::Error,
            ),
            alloc.reflow("In the rest of the program, I will only use the latter definition:"),
            alloc.region_all_the_things(
                record_region,
                field_region,
                field_region,
                Annotation::TypoSuggestion,
            ),
            alloc.concat(vec![
                alloc.reflow("For clarity, remove the previous "),
                alloc.record_field(field_name),
                alloc.reflow(" definitions from this record type."),
            ]),
        ]),
        Problem::DuplicateTag {
            tag_name,
            tag_union_region,
            tag_region,
            replaced_region,
        } => alloc.stack(vec![
            alloc.concat(vec![
                alloc.reflow("This tag union type defines the "),
                alloc.tag_name(tag_name.clone()),
                alloc.reflow(" tag twice!"),
            ]),
            alloc.region_all_the_things(
                tag_union_region,
                replaced_region,
                tag_region,
                Annotation::Error,
            ),
            alloc.reflow("In the rest of the program, I will only use the latter definition:"),
            alloc.region_all_the_things(
                tag_union_region,
                tag_region,
                tag_region,
                Annotation::TypoSuggestion,
            ),
            alloc.concat(vec![
                alloc.reflow("For clarity, remove the previous "),
                alloc.tag_name(tag_name),
                alloc.reflow(" definitions from this tag union type."),
            ]),
        ]),
        Problem::RuntimeError(runtime_error) => pretty_runtime_error(alloc, runtime_error),
    };

    Report {
        title: "SYNTAX PROBLEM".to_string(),
        filename,
        doc,
    }
}

fn not_found<'b>(
    alloc: &'b RocDocAllocator<'b>,
    region: roc_region::all::Region,
    name: &str,
    thing: &'b str,
    options: MutSet<Box<str>>,
) -> RocDocBuilder<'b> {
    use crate::type_error::suggest;

    let mut suggestions = suggest::sort(name, options.iter().map(|v| v.as_ref()).collect());
    suggestions.truncate(4);

    let default_no = alloc.concat(vec![
        alloc.reflow("Is there an "),
        alloc.keyword("import"),
        alloc.reflow(" or "),
        alloc.keyword("exposing"),
        alloc.reflow(" missing up-top"),
    ]);

    let default_yes = alloc.reflow("these names seem close though:");

    let to_details = |no_suggestion_details, yes_suggestion_details| {
        if suggestions.is_empty() {
            no_suggestion_details
        } else {
            alloc.stack(vec![
                yes_suggestion_details,
                alloc
                    .vcat(suggestions.into_iter().map(|v| alloc.string(v.to_string())))
                    .indent(4),
            ])
        }
    };

    alloc.stack(vec![
        alloc.concat(vec![
            alloc.reflow("I cannot find a `"),
            alloc.string(name.to_string()),
            alloc.reflow("` "),
            alloc.reflow(thing),
        ]),
        alloc.region(region),
        to_details(default_no, default_yes),
    ])
}
fn pretty_runtime_error<'b>(
    alloc: &'b RocDocAllocator<'b>,
    runtime_error: RuntimeError,
) -> RocDocBuilder<'b> {
    match runtime_error {
        RuntimeError::Shadowing {
            original_region,
            shadow,
        } => {
            let line = r#"Since these variables have the same name, it's easy to use the wrong one on accident. Give one of them a new name."#;

            alloc.stack(vec![
                alloc
                    .text("The ")
                    .append(alloc.ident(shadow.value))
                    .append(alloc.reflow(" name is first defined here:")),
                alloc.region(original_region),
                alloc.reflow("But then it's defined a second time here:"),
                alloc.region(shadow.region),
                alloc.reflow(line),
            ])
        }

        RuntimeError::LookupNotInScope(loc_name, options) => {
            not_found(alloc, loc_name.region, &loc_name.value, "value", options)
        }
        RuntimeError::CircularDef(mut idents, regions) => {
            let first = idents.remove(0);

            if idents.is_empty() {
                alloc
                    .reflow("The ")
                    .append(alloc.ident(first.value.clone()))
                    .append(alloc.reflow(
                        " value is defined directly in terms of itself, causing an infinite loop.",
                    ))
            // TODO "are you trying to mutate a variable?
            // TODO hint?
            } else {
                alloc.stack(vec![
                    alloc
                        .reflow("The ")
                        .append(alloc.ident(first.value.clone()))
                        .append(
                            alloc.reflow(" definition is causing a very tricky infinite loop:"),
                        ),
                    alloc.region(regions[0].0),
                    alloc
                        .reflow("The ")
                        .append(alloc.ident(first.value.clone()))
                        .append(alloc.reflow(
                            " value depends on itself through the following chain of definitions:",
                        )),
                    cycle(
                        alloc,
                        4,
                        alloc.ident(first.value),
                        idents
                            .into_iter()
                            .map(|ident| alloc.ident(ident.value))
                            .collect::<Vec<_>>(),
                    ),
                    // TODO hint?
                ])
            }
        }
        other => {
            //    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
            //    UnsupportedPattern(Region),
            //    UnrecognizedFunctionName(Located<InlinableString>),
            //    SymbolNotExposed {
            //        module_name: InlinableString,
            //        ident: InlinableString,
            //        region: Region,
            //    },
            //    ModuleNotImported {
            //        module_name: InlinableString,
            //        ident: InlinableString,
            //        region: Region,
            //    },
            //    InvalidPrecedence(PrecedenceProblem, Region),
            //    MalformedIdentifier(Box<str>, Region),
            //    MalformedClosure(Region),
            //    FloatOutsideRange(Box<str>),
            //    IntOutsideRange(Box<str>),
            //    InvalidHex(std::num::ParseIntError, Box<str>),
            //    InvalidOctal(std::num::ParseIntError, Box<str>),
            //    InvalidBinary(std::num::ParseIntError, Box<str>),
            //    QualifiedPatternIdent(InlinableString),
            //    CircularDef(
            //        Vec<Located<Ident>>,
            //        Vec<(Region /* pattern */, Region /* expr */)>,
            //    ),
            //
            //    /// When the author specifies a type annotation but no implementation
            //    NoImplementation,
            todo!("TODO implement run time error reporting for {:?}", other)
        }
    }
}

// define custom allocator struct so we can `impl RocDocAllocator` custom helpers
pub struct RocDocAllocator<'a> {
    upstream: BoxAllocator,
    src_lines: &'a [&'a str],
    pub home: ModuleId,
    pub interns: &'a Interns,
}

pub type RocDocBuilder<'b> = DocBuilder<'b, RocDocAllocator<'b>, Annotation>;

impl<'a, A> DocAllocator<'a, A> for RocDocAllocator<'a>
where
    A: 'a,
{
    type Doc = ven_pretty::BoxDoc<'a, A>;

    fn alloc(&'a self, doc: ven_pretty::Doc<'a, Self::Doc, A>) -> Self::Doc {
        self.upstream.alloc(doc)
    }

    fn alloc_column_fn(
        &'a self,
        f: impl Fn(usize) -> Self::Doc + 'a,
    ) -> <Self::Doc as ven_pretty::DocPtr<'a, A>>::ColumnFn {
        self.upstream.alloc_column_fn(f)
    }

    fn alloc_width_fn(
        &'a self,
        f: impl Fn(isize) -> Self::Doc + 'a,
    ) -> <Self::Doc as ven_pretty::DocPtr<'a, A>>::WidthFn {
        self.upstream.alloc_width_fn(f)
    }
}

impl<'a> RocDocAllocator<'a> {
    pub fn new(src_lines: &'a [&'a str], home: ModuleId, interns: &'a Interns) -> Self {
        RocDocAllocator {
            upstream: BoxAllocator,
            home,
            src_lines,
            interns,
        }
    }

    /// vertical concatenation. Adds a newline between elements
    pub fn vcat<A, I>(&'a self, docs: I) -> DocBuilder<'a, Self, A>
    where
        A: 'a + Clone,
        I: IntoIterator,
        I::Item: Into<ven_pretty::BuildDoc<'a, ven_pretty::BoxDoc<'a, A>, A>>,
    {
        self.intersperse(docs, self.line())
    }

    /// like vcat, but adds a double line break between elements. Visually this means an empty line
    /// between elements.
    pub fn stack<A, I>(&'a self, docs: I) -> DocBuilder<'a, Self, A>
    where
        A: 'a + Clone,
        I: IntoIterator,
        I::Item: Into<ven_pretty::BuildDoc<'a, ven_pretty::BoxDoc<'a, A>, A>>,
    {
        self.intersperse(docs, self.line().append(self.line()))
    }

    /// text from a String. Note that this does not reflow!
    pub fn string(&'a self, string: String) -> DocBuilder<'a, Self, Annotation> {
        let x: std::borrow::Cow<'a, str> = string.into();

        self.text(x)
    }

    pub fn keyword(&'a self, string: &'a str) -> DocBuilder<'a, Self, Annotation> {
        self.text(string).annotate(Annotation::Keyword)
    }

    pub fn type_str(&'a self, content: &str) -> DocBuilder<'a, Self, Annotation> {
        self.string(content.to_owned()).annotate(Annotation::Alias)
    }

    pub fn type_variable(&'a self, content: Lowercase) -> DocBuilder<'a, Self, Annotation> {
        // currently not annotated
        self.string(content.to_string())
            .annotate(Annotation::TypeVariable)
    }

    pub fn tag_name(&'a self, tn: TagName) -> DocBuilder<'a, Self, Annotation> {
        match tn {
            TagName::Global(uppercase) => self.global_tag_name(uppercase),
            TagName::Private(symbol) => self.private_tag_name(symbol),
        }
    }

    pub fn symbol_unqualified(&'a self, symbol: Symbol) -> DocBuilder<'a, Self, Annotation> {
        self.text(format!("{}", symbol.ident_string(self.interns)))
            .annotate(Annotation::Symbol)
    }
    pub fn symbol_foreign_qualified(&'a self, symbol: Symbol) -> DocBuilder<'a, Self, Annotation> {
        if symbol.module_id() == self.home || symbol.module_id().is_builtin() {
            // Render it unqualified if it's in the current module or a builtin
            self.text(format!("{}", symbol.ident_string(self.interns)))
                .annotate(Annotation::Symbol)
        } else {
            self.text(format!(
                "{}.{}",
                symbol.module_string(self.interns),
                symbol.ident_string(self.interns),
            ))
            .annotate(Annotation::Symbol)
        }
    }
    pub fn symbol_qualified(&'a self, symbol: Symbol) -> DocBuilder<'a, Self, Annotation> {
        self.text(format!(
            "{}.{}",
            symbol.module_string(self.interns),
            symbol.ident_string(self.interns),
        ))
        .annotate(Annotation::Symbol)
    }

    pub fn private_tag_name(&'a self, symbol: Symbol) -> DocBuilder<'a, Self, Annotation> {
        if symbol.module_id() == self.home {
            // Render it unqualified if it's in the current module.
            self.text(format!("{}", symbol.ident_string(self.interns)))
                .annotate(Annotation::PrivateTag)
        } else {
            self.text(format!(
                "{}.{}",
                symbol.module_string(self.interns),
                symbol.ident_string(self.interns),
            ))
            .annotate(Annotation::PrivateTag)
        }
    }

    pub fn global_tag_name(&'a self, uppercase: Uppercase) -> DocBuilder<'a, Self, Annotation> {
        self.text(format!("{}", uppercase))
            .annotate(Annotation::GlobalTag)
    }

    pub fn record_field(&'a self, lowercase: Lowercase) -> DocBuilder<'a, Self, Annotation> {
        self.text(format!(".{}", lowercase))
            .annotate(Annotation::RecordField)
    }

    pub fn module(&'a self, module_id: ModuleId) -> DocBuilder<'a, Self, Annotation> {
        self.text(format!("{}", self.interns.module_name(module_id)))
            .annotate(Annotation::Module)
    }

    pub fn binop(
        &'a self,
        content: roc_parse::operator::BinOp,
    ) -> DocBuilder<'a, Self, Annotation> {
        self.text(content.to_string()).annotate(Annotation::BinOp)
    }

    /// Turns of backticks/colors in a block
    pub fn type_block(
        &'a self,
        content: DocBuilder<'a, Self, Annotation>,
    ) -> DocBuilder<'a, Self, Annotation> {
        content.annotate(Annotation::TypeBlock).indent(4)
    }

    pub fn hint(&'a self) -> DocBuilder<'a, Self, Annotation> {
        self.text("Hint:")
            .append(self.softline())
            .annotate(Annotation::Hint)
    }

    pub fn region_all_the_things(
        &'a self,
        region: roc_region::all::Region,
        sub_region1: roc_region::all::Region,
        sub_region2: roc_region::all::Region,
        error_annotation: Annotation,
    ) -> DocBuilder<'a, Self, Annotation> {
        debug_assert!(region.contains(&sub_region1));
        debug_assert!(region.contains(&sub_region2));

        // if true, the final line of the snippet will be some ^^^ that point to the region where
        // the problem is. Otherwise, the snippet will have a > on the lines that are in the regon
        // where the problem is.
        let error_highlight_line = region.start_line == region.end_line;

        let max_line_number_length = (region.end_line + 1).to_string().len();
        let indent = 2;

        let mut result = self.nil();
        for i in region.start_line..=region.end_line {
            let line_number_string = (i + 1).to_string();
            let line_number = line_number_string;
            let this_line_number_length = line_number.len();

            let line = self.src_lines[i as usize];

            let rest_of_line = if !line.trim().is_empty() {
                self.text(line).indent(indent)
            } else {
                self.nil()
            };

            let highlight = !error_highlight_line
                && ((i >= sub_region1.start_line && i <= sub_region1.end_line)
                    || (i >= sub_region2.start_line && i <= sub_region2.end_line));

            let source_line = if highlight {
                self.text(" ".repeat(max_line_number_length - this_line_number_length))
                    .append(self.text(line_number).annotate(Annotation::LineNumber))
                    .append(self.text(GUTTER_BAR).annotate(Annotation::GutterBar))
                    .append(self.text(">").annotate(error_annotation))
                    .append(rest_of_line)
            } else if error_highlight_line {
                self.text(" ".repeat(max_line_number_length - this_line_number_length))
                    .append(self.text(line_number).annotate(Annotation::LineNumber))
                    .append(self.text(GUTTER_BAR).annotate(Annotation::GutterBar))
                    .append(rest_of_line)
            } else {
                self.text(" ".repeat(max_line_number_length - this_line_number_length))
                    .append(self.text(line_number).annotate(Annotation::LineNumber))
                    .append(self.text(GUTTER_BAR).annotate(Annotation::GutterBar))
                    .append(self.text(" "))
                    .append(rest_of_line)
            };

            result = result.append(source_line);

            if i != region.end_line {
                result = result.append(self.line())
            }
        }

        if error_highlight_line {
            let overlapping = sub_region2.start_col < sub_region1.end_col;

            let highlight = if overlapping {
                self.text("^".repeat((sub_region2.end_col - sub_region1.start_col) as usize))
            } else {
                let highlight1 = "^".repeat((sub_region1.end_col - sub_region1.start_col) as usize);
                let highlight2 = if sub_region1 == sub_region2 {
                    "".repeat(0)
                } else {
                    "^".repeat((sub_region2.end_col - sub_region2.start_col) as usize)
                };
                let inbetween = " "
                    .repeat((sub_region2.start_col.saturating_sub(sub_region1.end_col)) as usize);

                self.text(highlight1)
                    .append(self.text(inbetween))
                    .append(self.text(highlight2))
            };

            let highlight_line = self
                .line()
                .append(self.text(" ".repeat(max_line_number_length)))
                .append(self.text(GUTTER_BAR).annotate(Annotation::GutterBar))
                .append(if sub_region1.is_empty() && sub_region2.is_empty() {
                    self.nil()
                } else {
                    self.text(" ".repeat(sub_region1.start_col as usize))
                        .indent(indent)
                        .append(highlight)
                        .annotate(error_annotation)
                });

            result = result.append(highlight_line);
        }

        result.annotate(Annotation::CodeBlock)
    }

    pub fn region_with_subregion(
        &'a self,
        region: roc_region::all::Region,
        sub_region: roc_region::all::Region,
    ) -> DocBuilder<'a, Self, Annotation> {
        debug_assert!(region.contains(&sub_region));

        // If the outer region takes more than 1 full screen (~60 lines), only show the inner region
        if region.end_line - region.start_line > 60 {
            return self.region_with_subregion(sub_region, sub_region);
        }

        // if true, the final line of the snippet will be some ^^^ that point to the region where
        // the problem is. Otherwise, the snippet will have a > on the lines that are in the regon
        // where the problem is.
        let error_highlight_line = sub_region.start_line == region.end_line;

        let max_line_number_length = (region.end_line + 1).to_string().len();
        let indent = 2;

        let mut result = self.nil();
        for i in region.start_line..=region.end_line {
            let line_number_string = (i + 1).to_string();
            let line_number = line_number_string;
            let this_line_number_length = line_number.len();

            let line = self.src_lines[i as usize];

            let rest_of_line = if !line.trim().is_empty() {
                self.text(line)
                    .annotate(Annotation::CodeBlock)
                    .indent(indent)
            } else {
                self.nil()
            };

            let source_line = if !error_highlight_line
                && i >= sub_region.start_line
                && i <= sub_region.end_line
            {
                self.text(" ".repeat(max_line_number_length - this_line_number_length))
                    .append(self.text(line_number).annotate(Annotation::LineNumber))
                    .append(self.text(GUTTER_BAR).annotate(Annotation::GutterBar))
                    .append(self.text(">").annotate(Annotation::Error))
                    .append(rest_of_line)
            } else if error_highlight_line {
                self.text(" ".repeat(max_line_number_length - this_line_number_length))
                    .append(self.text(line_number).annotate(Annotation::LineNumber))
                    .append(self.text(GUTTER_BAR).annotate(Annotation::GutterBar))
                    .append(rest_of_line)
            } else {
                self.text(" ".repeat(max_line_number_length - this_line_number_length))
                    .append(self.text(line_number).annotate(Annotation::LineNumber))
                    .append(self.text(GUTTER_BAR).annotate(Annotation::GutterBar))
                    .append(self.text(" "))
                    .append(rest_of_line)
            };

            result = result.append(source_line);

            if i != region.end_line {
                result = result.append(self.line())
            }
        }

        if error_highlight_line {
            let highlight_text = "^".repeat((sub_region.end_col - sub_region.start_col) as usize);
            let highlight_line = self
                .line()
                .append(self.text(" ".repeat(max_line_number_length)))
                .append(self.text(GUTTER_BAR).annotate(Annotation::GutterBar))
                .append(if highlight_text.is_empty() {
                    self.nil()
                } else {
                    self.text(" ".repeat(sub_region.start_col as usize))
                        .indent(indent)
                        .append(self.text(highlight_text).annotate(Annotation::Error))
                });

            result = result.append(highlight_line);
        }

        result
    }

    pub fn region(&'a self, region: roc_region::all::Region) -> DocBuilder<'a, Self, Annotation> {
        self.region_with_subregion(region, region)
    }

    pub fn region_without_error(
        &'a self,
        region: roc_region::all::Region,
    ) -> DocBuilder<'a, Self, Annotation> {
        let mut result = self.nil();
        for i in region.start_line..=region.end_line {
            let line = if i == region.start_line {
                if i == region.end_line {
                    &self.src_lines[i as usize][region.start_col as usize..region.end_col as usize]
                } else {
                    &self.src_lines[i as usize][region.start_col as usize..]
                }
            } else if i == region.end_line {
                &self.src_lines[i as usize][0..region.end_col as usize]
            } else {
                self.src_lines[i as usize]
            };

            let rest_of_line = if !line.trim().is_empty() {
                self.text(line).annotate(Annotation::CodeBlock)
            } else {
                self.nil()
            };

            result = result.append(rest_of_line);

            if i != region.end_line {
                result = result.append(self.line())
            }
        }

        result.indent(4)
    }

    pub fn ident(&'a self, ident: Ident) -> DocBuilder<'a, Self, Annotation> {
        self.text(format!("{}", ident.as_inline_str()))
            .annotate(Annotation::Symbol)
    }
}

#[derive(Copy, Clone)]
pub enum Annotation {
    Emphasized,
    Url,
    Keyword,
    GlobalTag,
    PrivateTag,
    RecordField,
    TypeVariable,
    Alias,
    Structure,
    Symbol,
    BinOp,
    Error,
    GutterBar,
    LineNumber,
    PlainText,
    CodeBlock,
    TypeBlock,
    Module,
    Typo,
    TypoSuggestion,
    Hint,
}

/// Render with minimal formatting
pub struct CiWrite<W> {
    style_stack: Vec<Annotation>,
    in_type_block: bool,
    in_code_block: bool,
    upstream: W,
}

impl<W> CiWrite<W> {
    pub fn new(upstream: W) -> CiWrite<W> {
        CiWrite {
            style_stack: vec![],
            in_type_block: false,
            in_code_block: false,
            upstream,
        }
    }
}

/// Render with fancy formatting
pub struct ColorWrite<'a, W> {
    style_stack: Vec<Annotation>,
    palette: &'a Palette<'a>,
    upstream: W,
}

impl<'a, W> ColorWrite<'a, W> {
    pub fn new(palette: &'a Palette, upstream: W) -> ColorWrite<'a, W> {
        ColorWrite {
            style_stack: vec![],
            palette,
            upstream,
        }
    }
}

impl<W> Render for CiWrite<W>
where
    W: fmt::Write,
{
    type Error = fmt::Error;

    fn write_str(&mut self, s: &str) -> Result<usize, fmt::Error> {
        self.write_str_all(s).map(|_| s.len())
    }

    fn write_str_all(&mut self, s: &str) -> fmt::Result {
        self.upstream.write_str(s)
    }
}

impl<W> RenderAnnotated<Annotation> for CiWrite<W>
where
    W: fmt::Write,
{
    fn push_annotation(&mut self, annotation: &Annotation) -> Result<(), Self::Error> {
        use Annotation::*;
        match annotation {
            TypeBlock => {
                self.in_type_block = true;
            }
            CodeBlock => {
                self.in_code_block = true;
            }
            Emphasized => {
                self.write_str("*")?;
            }
            Url => {
                self.write_str("<")?;
            }
            GlobalTag | PrivateTag | Keyword | RecordField | Symbol | Typo | TypoSuggestion
            | TypeVariable
                if !self.in_type_block && !self.in_code_block =>
            {
                self.write_str("`")?;
            }

            _ => {}
        }
        self.style_stack.push(*annotation);
        Ok(())
    }

    fn pop_annotation(&mut self) -> Result<(), Self::Error> {
        use Annotation::*;

        match self.style_stack.pop() {
            None => {}
            Some(annotation) => match annotation {
                TypeBlock => {
                    self.in_type_block = false;
                }
                CodeBlock => {
                    self.in_code_block = false;
                }
                Emphasized => {
                    self.write_str("*")?;
                }
                Url => {
                    self.write_str(">")?;
                }
                GlobalTag | PrivateTag | Keyword | RecordField | Symbol | Typo | TypoSuggestion
                | TypeVariable
                    if !self.in_type_block && !self.in_code_block =>
                {
                    self.write_str("`")?;
                }

                _ => {}
            },
        }
        Ok(())
    }
}

impl<'a, W> Render for ColorWrite<'a, W>
where
    W: fmt::Write,
{
    type Error = fmt::Error;

    fn write_str(&mut self, s: &str) -> Result<usize, fmt::Error> {
        self.write_str_all(s).map(|_| s.len())
    }

    fn write_str_all(&mut self, s: &str) -> fmt::Result {
        self.upstream.write_str(s)
    }
}

impl<'a, W> RenderAnnotated<Annotation> for ColorWrite<'a, W>
where
    W: fmt::Write,
{
    fn push_annotation(&mut self, annotation: &Annotation) -> Result<(), Self::Error> {
        use Annotation::*;
        match annotation {
            Emphasized => {
                self.write_str(BOLD_CODE)?;
            }
            Url | Hint => {
                self.write_str(UNDERLINE_CODE)?;
            }
            PlainText => {
                self.write_str(self.palette.primary)?;
            }
            CodeBlock => {
                self.write_str(self.palette.code_block)?;
            }
            TypeVariable => {
                self.write_str(self.palette.type_variable)?;
            }
            Alias => {
                self.write_str(self.palette.alias)?;
            }
            BinOp => {
                self.write_str(self.palette.alias)?;
            }
            Symbol => {
                self.write_str(self.palette.variable)?;
            }
            GutterBar => {
                self.write_str(self.palette.gutter_bar)?;
            }
            Error => {
                self.write_str(self.palette.error)?;
            }
            LineNumber => {
                self.write_str(self.palette.line_number)?;
            }
            Structure => {
                self.write_str(self.palette.structure)?;
            }
            Module => {
                self.write_str(self.palette.module_name)?;
            }
            Typo => {
                self.write_str(self.palette.typo)?;
            }
            TypoSuggestion => {
                self.write_str(self.palette.typo_suggestion)?;
            }
            TypeBlock | GlobalTag | PrivateTag | RecordField | Keyword => { /* nothing yet */ }
        }
        self.style_stack.push(*annotation);
        Ok(())
    }

    fn pop_annotation(&mut self) -> Result<(), Self::Error> {
        use Annotation::*;

        match self.style_stack.pop() {
            None => {}
            Some(annotation) => match annotation {
                Emphasized | Url | TypeVariable | Alias | Symbol | BinOp | Error | GutterBar
                | Typo | TypoSuggestion | Structure | CodeBlock | PlainText | LineNumber | Hint
                | Module => {
                    self.write_str(RESET_CODE)?;
                }

                TypeBlock | GlobalTag | PrivateTag | RecordField | Keyword => { /* nothing yet */ }
            },
        }
        Ok(())
    }
}
