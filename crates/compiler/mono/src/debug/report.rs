use std::fmt::Display;

use roc_module::symbol::{Interns, Symbol};
use ven_pretty::{text, Arena, DocAllocator, DocBuilder};

use crate::{
    ir::{Parens, ProcLayout},
    layout::LayoutInterner,
};

use super::{
    checker::{ProblemKind, UseKind},
    Problem, Problems,
};

pub fn format_problems<'a, I>(
    interns: &Interns,
    interner: &I,
    problems: Problems<'a>,
) -> impl Display
where
    I: LayoutInterner<'a>,
{
    let Problems(problems) = problems;
    let f = Arena::new();
    let problem_docs = problems
        .into_iter()
        .map(|p| format_problem(&f, interns, interner, p));
    let all = f.intersperse(problem_docs, f.hardline());
    all.1.pretty(80).to_string()
}

type Doc<'d> = DocBuilder<'d, Arena<'d>>;

const GUTTER_BAR: &str = "│";
const HEADER_WIDTH: usize = 80;

fn format_problem<'a, 'd, I>(
    f: &'d Arena<'d>,
    interns: &'d Interns,
    interner: &'d I,
    problem: Problem<'a>,
) -> Doc<'d>
where
    'a: 'd,
    I: LayoutInterner<'a>,
{
    let Problem {
        proc,
        proc_layout,
        line,
        kind,
    } = problem;

    let (title, mut docs, last_doc) = format_kind(f, interns, interner, kind);
    docs.push((line, last_doc));
    docs.sort_by_key(|(line, _)| *line);

    let src = proc
        .to_doc(f, interner, true, Parens::NotNeeded)
        .1
        .pretty(80)
        .to_string();

    eprintln!("Full source: {src}");

    let interpolated_docs = stack(
        f,
        docs.into_iter()
            .map(|(line, doc)| format_sourced_doc(f, line, &src, doc)),
    );

    let header = format_header(f, title);
    let proc_loc = format_proc_spec(f, interns, interner, proc.name.name(), proc_layout);

    stack(
        f,
        [
            header,
            f.concat([f.reflow("in "), proc_loc]),
            interpolated_docs,
        ],
    )
}

fn format_sourced_doc<'d>(f: &'d Arena<'d>, line: usize, source: &str, doc: Doc<'d>) -> Doc<'d> {
    let start_at = line.saturating_sub(1);
    let source_lines = source.lines().skip(start_at).take(3);
    let max_line_no_width = (start_at.to_string().len()).max((start_at + 3).to_string().len());
    let pretty_lines = source_lines.enumerate().map(|(i, line_src)| {
        let line_no = start_at + i;
        let line_no_s = line_no.to_string();
        let line_no_len = line_no_s.len();
        f.text(line_no_s)
            .append(f.text(" ".repeat(max_line_no_width - line_no_len)))
            .append(f.text(GUTTER_BAR))
            .append(f.text(if line_no == line { "> " } else { "  " }))
            .append(f.text(line_src.to_string()))
    });
    let pretty_lines = f.intersperse(pretty_lines, f.hardline());
    stack(f, [pretty_lines, doc])
}

fn format_header<'d>(f: &'d Arena<'d>, title: &str) -> Doc<'d> {
    let title_width = title.len() + 4;
    text!(f, "── {} {}", title, "─".repeat(HEADER_WIDTH - title_width))
}

fn format_kind<'a, 'd, I>(
    f: &'d Arena<'d>,
    interns: &'d Interns,
    interner: &I,
    kind: ProblemKind<'a>,
) -> (&'static str, Vec<(usize, Doc<'d>)>, Doc<'d>)
where
    I: LayoutInterner<'a>,
{
    let title;
    let docs_before;
    let doc = match kind {
        ProblemKind::RedefinedSymbol { symbol, old_line } => {
            title = "REDEFINED SYMBOL";
            docs_before = vec![(
                old_line,
                f.concat([
                    format_symbol(f, interns, symbol),
                    f.reflow(" first defined here"),
                ]),
            )];
            f.concat([
                format_symbol(f, interns, symbol),
                f.reflow(" re-defined here"),
            ])
        }
        ProblemKind::NoSymbolInScope { symbol } => {
            title = "SYMBOL NOT DEFINED";
            docs_before = vec![];
            f.concat([
                format_symbol(f, interns, symbol),
                f.reflow(" not found in the present scope"),
            ])
        }
        ProblemKind::SymbolUseMismatch {
            symbol,
            def_layout,
            def_line,
            use_layout,
            use_kind,
        } => {
            title = "SYMBOL LAYOUT DOESN'T MATCH ITS USE";
            docs_before = vec![(
                def_line,
                f.concat([
                    format_symbol(f, interns, symbol),
                    f.reflow(" defined here with layout "),
                    interner.to_doc_top(def_layout, f),
                ]),
            )];
            f.concat([
                format_symbol(f, interns, symbol),
                f.reflow(" used as a "),
                f.reflow(format_use_kind(use_kind)),
                f.reflow(" here with layout "),
                interner.to_doc_top(use_layout, f),
            ])
        }
        ProblemKind::SymbolDefMismatch {
            symbol,
            def_layout,
            expr_layout,
        } => {
            title = "SYMBOL INITIALIZER HAS THE WRONG LAYOUT";
            docs_before = vec![];
            f.concat([
                format_symbol(f, interns, symbol),
                f.reflow(" is defined as "),
                interner.to_doc_top(def_layout, f),
                f.reflow(" but its initializer is "),
                interner.to_doc_top(expr_layout, f),
            ])
        }
        ProblemKind::BadSwitchConditionLayout { found_layout } => {
            title = "BAD SWITCH CONDITION LAYOUT";
            docs_before = vec![];
            f.concat([
                f.reflow("This switch condition is a "),
                interner.to_doc_top(found_layout, f),
            ])
        }
        ProblemKind::DuplicateSwitchBranch {} => {
            title = "DUPLICATE SWITCH BRANCH";
            docs_before = vec![];
            f.reflow("The match of switch branch is reached earlier")
        }
        ProblemKind::RedefinedJoinPoint { id, old_line } => {
            title = "DUPLICATE JOIN POINT";
            docs_before = vec![(
                old_line,
                f.concat([
                    f.reflow("The join point "),
                    f.as_string(id.0),
                    f.reflow(" was previously defined here"),
                ]),
            )];
            f.reflow("and is redefined here")
        }
        ProblemKind::NoJoinPoint { id } => {
            title = "JOIN POINT NOT DEFINED";
            docs_before = vec![];
            f.concat([
                f.reflow("The join point "),
                f.as_string(id.0),
                f.reflow(" was not found in the present scope"),
            ])
        }
        ProblemKind::JumpArityMismatch {
            def_line,
            num_needed,
            num_given,
        } => {
            title = "WRONG NUMBER OF ARGUMENTS IN JUMP";
            docs_before = vec![(
                def_line,
                f.concat([
                    f.reflow("This join pont needs "),
                    f.as_string(num_needed),
                    f.reflow(" arguments"),
                ]),
            )];
            f.concat([
                f.reflow("but this jump only gives it "),
                f.as_string(num_given),
            ])
        }
        ProblemKind::CallingUndefinedProc {
            symbol,
            proc_layout,
            similar,
        } => {
            title = "PROC SPECIALIZATION NOT DEFINED";
            docs_before = vec![];
            let no_spec_doc = stack(
                f,
                [
                    f.reflow("No specialization"),
                    format_proc_spec(f, interns, interner, symbol, proc_layout),
                    f.reflow("was found"),
                ],
            );
            let similar_doc = if similar.is_empty() {
                f.nil()
            } else {
                let similars = similar
                    .into_iter()
                    .map(|other| format_proc_spec(f, interns, interner, symbol, other));
                stack(
                    f,
                    [f.concat([
                        f.reflow("The following specializations of "),
                        format_symbol(f, interns, symbol),
                        f.reflow(" were built:"),
                        stack(f, similars),
                    ])],
                )
            };
            stack(f, [no_spec_doc, similar_doc])
        }
        ProblemKind::DuplicateCallSpecId { old_call_line } => {
            title = "DUPLICATE CALL SPEC ID";
            docs_before = vec![(old_call_line, f.reflow("This call has a specialization ID"))];
            f.reflow("...that is the same as the specialization ID of the call here")
        }
        ProblemKind::StructIndexOOB {
            structure,
            def_line,
            index,
            size,
        } => {
            title = "STRUCT INDEX IS OUT-OF-BOUNDS";
            docs_before = vec![(
                def_line,
                f.concat([
                    f.reflow("The struct "),
                    format_symbol(f, interns, structure),
                    f.reflow(" defined here has "),
                    f.as_string(size),
                    f.reflow(" fields"),
                ]),
            )];
            f.concat([
                f.reflow("but is being indexed into field "),
                f.as_string(index),
            ])
        }
        ProblemKind::NotAStruct {
            structure,
            def_line,
        } => {
            title = "SYMBOL IS NOT A STRUCT";
            docs_before = vec![(
                def_line,
                f.concat([
                    f.reflow("The value "),
                    format_symbol(f, interns, structure),
                    f.reflow(" defined here"),
                ]),
            )];
            f.reflow("cannot be used as a structure here")
        }
        ProblemKind::IndexingTagIdNotInUnion {
            structure,
            def_line,
            tag_id,
            union_layout,
        } => {
            title = "TAG ID NOT IN UNION";
            docs_before = vec![(
                def_line,
                f.concat([
                    f.reflow("The union "),
                    format_symbol(f, interns, structure),
                    f.reflow(" defined here has layout "),
                    interner.to_doc_top(union_layout, f),
                ]),
            )];
            f.concat([f.reflow("which has no tag of id "), f.as_string(tag_id)])
        }
        ProblemKind::TagUnionStructIndexOOB {
            structure,
            def_line,
            tag_id,
            index,
            size,
        } => {
            title = "UNION ID AND PAYLOAD INDEX IS OUT-OF-BOUNDS";
            docs_before = vec![(
                def_line,
                f.concat([
                    f.reflow("The union "),
                    format_symbol(f, interns, structure),
                    f.reflow(" defined here has "),
                    f.as_string(size),
                    f.reflow(" payloads at ID "),
                    f.as_string(tag_id),
                ]),
            )];
            f.concat([
                f.reflow("but is being indexed into field "),
                f.as_string(index),
                f.reflow(" here"),
            ])
        }
        ProblemKind::IndexIntoNullableTag {
            structure,
            def_line,
            tag_id,
            union_layout,
        } => {
            title = "INDEX INTO NULLABLE TAG";
            docs_before = vec![(
                def_line,
                f.concat([
                    f.reflow("The union "),
                    format_symbol(f, interns, structure),
                    f.reflow(" defined here has layout "),
                    interner.to_doc_top(union_layout, f),
                ]),
            )];
            f.concat([
                f.reflow("but is being indexed into the nullable variant "),
                f.as_string(tag_id),
                f.reflow(" here"),
            ])
        }
        ProblemKind::UnboxNotABox { symbol, def_line } => {
            title = "ATTEMPTING TO UNBOX A NON-BOX";
            docs_before = vec![(
                def_line,
                f.concat([format_symbol(f, interns, symbol), f.reflow(" is not a box")]),
            )];
            f.reflow("but is being unboxed here")
        }
        ProblemKind::CreatingTagIdNotInUnion {
            tag_id,
            union_layout,
        } => {
            title = "NO SUCH ID FOR TAG UNION";
            docs_before = vec![];
            f.concat([
                f.reflow("The variant "),
                f.as_string(tag_id),
                f.reflow(" is outside the target union layout "),
                interner.to_doc_top(union_layout, f),
            ])
        }
        ProblemKind::CreateTagPayloadMismatch {
            num_needed,
            num_given,
        } => {
            title = "WRONG NUMBER OF ARGUMENTS IN TAG UNION";
            docs_before = vec![];
            f.concat([
                f.reflow("This tag union payload needs "),
                f.as_string(num_needed),
                f.reflow(" values, but is only given "),
                f.as_string(num_given),
            ])
        }
    };
    (title, docs_before, doc)
}

fn format_symbol<'d>(f: &'d Arena<'d>, interns: &'d Interns, symbol: Symbol) -> Doc<'d> {
    f.text(symbol.module_string(interns).to_string())
        .append(f.text("."))
        .append(f.text(symbol.as_str(interns)))
}

fn format_use_kind(use_kind: UseKind) -> &'static str {
    match use_kind {
        UseKind::Ret => "return value",
        UseKind::TagExpr => "tag constructor",
        UseKind::TagReuse => "tag reuse",
        UseKind::TagPayloadArg => "tag's payload",
        UseKind::ListElemExpr => "list element",
        UseKind::CallArg => "call argument",
        UseKind::JumpArg => "jump argument",
        UseKind::CrashArg => "crash message",
        UseKind::SwitchCond => "switch condition",
        UseKind::ExpectCond => "expect condition",
        UseKind::ExpectLookup => "lookup for an expect",
    }
}

fn format_proc_spec<'a, 'd, I>(
    f: &'d Arena<'d>,
    interns: &'d Interns,
    interner: &I,
    symbol: Symbol,
    proc_layout: ProcLayout<'a>,
) -> Doc<'d>
where
    I: LayoutInterner<'a>,
{
    f.concat([
        f.as_string(symbol.as_str(interns)),
        f.reflow(" : "),
        format_proc_layout(f, interner, proc_layout),
    ])
}

fn format_proc_layout<'a, 'd, I>(
    f: &'d Arena<'d>,
    interner: &I,
    proc_layout: ProcLayout<'a>,
) -> Doc<'d>
where
    I: LayoutInterner<'a>,
{
    let ProcLayout {
        arguments,
        result,
        niche: captures_niche,
    } = proc_layout;
    let args = f.intersperse(
        arguments
            .iter()
            .map(|a| interner.to_doc(*a, f, &mut Default::default(), Parens::InFunction)),
        f.reflow(", "),
    );
    let fun = f.concat([
        f.concat([f.reflow("("), args, f.reflow(")")]),
        f.reflow(" -> "),
        interner.to_doc_top(result, f),
    ]);
    let niche = (f.text("("))
        .append(captures_niche.to_doc(f, interner, &mut Default::default()))
        .append(f.text(")"));
    f.concat([fun, f.space(), niche])
}

fn stack<'d>(f: &'d Arena<'d>, docs: impl IntoIterator<Item = Doc<'d>>) -> Doc<'d> {
    f.intersperse(docs, f.line().append(f.line()))
}
