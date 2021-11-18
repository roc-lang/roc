use crate::report::{Annotation, Report, RocDocAllocator, RocDocBuilder, Severity};
use std::path::PathBuf;
use ven_pretty::DocAllocator;

pub fn mono_problem<'b>(
    alloc: &'b RocDocAllocator<'b>,
    filename: PathBuf,
    problem: roc_mono::ir::MonoProblem,
) -> Report<'b> {
    use roc_mono::exhaustive::Context::*;
    use roc_mono::exhaustive::Error::*;
    use roc_mono::ir::MonoProblem::*;

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
                    severity: Severity::RuntimeError,
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
                severity: Severity::Warning,
            }
        }
    }
}

pub fn unhandled_patterns_to_doc_block<'b>(
    alloc: &'b RocDocAllocator<'b>,
    patterns: Vec<roc_mono::exhaustive::Pattern>,
) -> RocDocBuilder<'b> {
    alloc
        .vcat(patterns.into_iter().map(|v| pattern_to_doc(alloc, v)))
        .indent(4)
        .annotate(Annotation::TypeBlock)
}

fn pattern_to_doc<'b>(
    alloc: &'b RocDocAllocator<'b>,
    pattern: roc_mono::exhaustive::Pattern,
) -> RocDocBuilder<'b> {
    pattern_to_doc_help(alloc, pattern, false)
}

fn pattern_to_doc_help<'b>(
    alloc: &'b RocDocAllocator<'b>,
    pattern: roc_mono::exhaustive::Pattern,
    in_type_param: bool,
) -> RocDocBuilder<'b> {
    use roc_mono::exhaustive::Literal::*;
    use roc_mono::exhaustive::Pattern::*;
    use roc_mono::exhaustive::RenderAs;

    match pattern {
        Anything => alloc.text("_"),
        Literal(l) => match l {
            Int(i) => alloc.text(i.to_string()),
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
                RenderAs::Guard => panic!("can this happen? inform Folkert"),
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
                RenderAs::Tag => {
                    let has_args = !args.is_empty();
                    let arg_docs = args
                        .into_iter()
                        .map(|v| pattern_to_doc_help(alloc, v, true));

                    let tag = &union.alternatives[tag_id.0 as usize];
                    let tag_name = tag.name.clone();

                    // We assume the alternatives are sorted. If not, this assert will trigger
                    debug_assert!(tag_id == tag.tag_id);

                    let docs = std::iter::once(alloc.tag_name(tag_name)).chain(arg_docs);

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
