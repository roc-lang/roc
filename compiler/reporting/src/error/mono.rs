use crate::report::{Annotation, Report, RocDocAllocator, RocDocBuilder};
use std::path::PathBuf;
use ven_pretty::DocAllocator;

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
        Ctor(union, tag_id, args) => {
            let arg_docs = args.into_iter().map(|v| pattern_to_doc(alloc, v));

            let tag = &union.alternatives[tag_id.0 as usize];
            let tag_name = tag.name.clone();

            // We assume the alternatives are sorted. If not, this assert will trigger
            debug_assert!(tag_id == tag.tag_id);

            let docs = std::iter::once(alloc.tag_name(tag_name)).chain(arg_docs);

            alloc.intersperse(docs, alloc.space())
        }
    }
}
