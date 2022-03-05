use crate::report::{Annotation, Report, RocDocAllocator, RocDocBuilder, Severity};
use roc_module::ident::TagName;
use roc_region::all::LineInfo;
use std::path::PathBuf;
use ven_pretty::DocAllocator;

pub fn mono_problem<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    filename: PathBuf,
    problem: roc_mono::ir::MonoProblem,
) -> Report<'b> {
    use roc_exhaustive::Context::*;
    use roc_exhaustive::Error::*;
    use roc_mono::ir::MonoProblem::*;

    match problem {
        PatternProblem(Incomplete(region, context, missing)) => match context {
            BadArg => {
                let doc = alloc.stack(vec![
                    alloc.reflow("This pattern does not cover all the possibilities:"),
                    alloc.region(lines.convert_region(region)),
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
                    severity: Severity::RuntimeError,
                }
            }
            BadDestruct => {
                let doc = alloc.stack(vec![
                    alloc.reflow("This pattern does not cover all the possibilities:"),
                    alloc.region(lines.convert_region(region)),
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
                    severity: Severity::RuntimeError,
                }
            }
            BadCase => {
                let doc = alloc.stack(vec![
                    alloc.concat(vec![
                        alloc.reflow("This "),
                        alloc.keyword("when"),
                        alloc.reflow(" does not cover all the possibilities:"),
                    ]),
                    alloc.region(lines.convert_region(region)),
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
                    severity: Severity::RuntimeError,
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
                alloc.region_with_subregion(
                    lines.convert_region(overall_region),
                    lines.convert_region(branch_region),
                ),
                alloc.reflow(
                    "Any value of this shape will be handled by \
                a previous pattern, so this one should be removed.",
                ),
            ]);

            Report {
                filename,
                title: "REDUNDANT PATTERN".to_string(),
                doc,
                severity: Severity::Warning,
            }
        }
    }
}

pub fn unhandled_patterns_to_doc_block<'b>(
    alloc: &'b RocDocAllocator<'b>,
    patterns: Vec<roc_exhaustive::Pattern>,
) -> RocDocBuilder<'b> {
    alloc
        .vcat(patterns.into_iter().map(|v| pattern_to_doc(alloc, v)))
        .indent(4)
        .annotate(Annotation::TypeBlock)
}

fn pattern_to_doc<'b>(
    alloc: &'b RocDocAllocator<'b>,
    pattern: roc_exhaustive::Pattern,
) -> RocDocBuilder<'b> {
    pattern_to_doc_help(alloc, pattern, false)
}

fn pattern_to_doc_help<'b>(
    alloc: &'b RocDocAllocator<'b>,
    pattern: roc_exhaustive::Pattern,
    in_type_param: bool,
) -> RocDocBuilder<'b> {
    use roc_exhaustive::Literal::*;
    use roc_exhaustive::Pattern::*;
    use roc_exhaustive::RenderAs;

    match pattern {
        Anything => alloc.text("_"),
        Literal(l) => match l {
            Int(i) => alloc.text(i.to_string()),
            U128(i) => alloc.text(i.to_string()),
            Bit(true) => alloc.text("True"),
            Bit(false) => alloc.text("False"),
            Byte(b) => alloc.text(b.to_string()),
            Float(f) => alloc.text(f.to_string()),
            // TODO: Proper Dec.to_str
            Decimal(d) => alloc.text(d.0.to_string()),
            Str(s) => alloc.string(s.into()),
        },
        Ctor(union, tag_id, args) => {
            match union.render_as {
                RenderAs::Guard => {
                    // #Guard <fake-condition-tag> <unexhausted-pattern>
                    debug_assert_eq!(
                        union.alternatives[tag_id.0 as usize].name,
                        TagName::Global("#Guard".into())
                    );
                    debug_assert!(args.len() == 2);
                    let tag = pattern_to_doc_help(alloc, args[1].clone(), in_type_param);
                    tag.append("    (note the lack of a guard)")
                }
                RenderAs::Record(field_names) => {
                    let mut arg_docs = Vec::with_capacity(args.len());

                    for (label, v) in field_names.into_iter().zip(args.into_iter()) {
                        match &v {
                            Anything => {
                                arg_docs.push(alloc.text(label.to_string()));
                            }
                            Literal(_) | Ctor(_, _, _) => {
                                arg_docs.push(
                                    alloc
                                        .text(label.to_string())
                                        .append(alloc.reflow(": "))
                                        .append(pattern_to_doc_help(alloc, v, false)),
                                );
                            }
                        }
                    }

                    alloc
                        .text("{ ")
                        .append(alloc.intersperse(arg_docs, alloc.reflow(", ")))
                        .append(" }")
                }
                RenderAs::Tag | RenderAs::Opaque => {
                    let has_args = !args.is_empty();
                    let arg_docs = args
                        .into_iter()
                        .map(|v| pattern_to_doc_help(alloc, v, true));

                    let tag = &union.alternatives[tag_id.0 as usize];
                    let tag_name = match union.render_as {
                        RenderAs::Tag => alloc.tag_name(tag.name.clone()),
                        RenderAs::Opaque => match tag.name {
                            TagName::Private(opaque) => alloc.wrapped_opaque_name(opaque),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };

                    // We assume the alternatives are sorted. If not, this assert will trigger
                    debug_assert!(tag_id == tag.tag_id);

                    let docs = std::iter::once(tag_name).chain(arg_docs);

                    if in_type_param && has_args {
                        alloc
                            .text("(")
                            .append(alloc.intersperse(docs, alloc.space()))
                            .append(")")
                    } else {
                        alloc.intersperse(docs, alloc.space())
                    }
                }
            }
        }
    }
}
