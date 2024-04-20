#![allow(clippy::redundant_closure_call)]
//|> clippy false positive: https://github.com/rust-lang/rust-clippy/issues/1553

pub fn merge_instructions_without_line_numbers(instructions: capstone::Instructions) -> String {
    instructions
        .iter()
        .map(|inst| {
            inst.to_string()
                .trim()
                .split(' ')
                .skip(1)
                .map(|x| x.trim())
                .collect::<std::vec::Vec<&str>>()
                .join(" ")
        })
        .collect::<std::vec::Vec<String>>()
        .join("\n")
}

#[macro_export]
macro_rules! disassembler_test {
    // TODO: Not sure if there is a better way to merge these together,
    // but I like the end use of this a lot better than the old tests.
    ($assemble_fn: expr, $format_fn: expr) => {{
        use $crate::generic64::disassembler_test_macro::merge_instructions_without_line_numbers;
        let arena = bumpalo::Bump::new();
        let (mut buf, cs) = setup_capstone_and_arena(&arena);
        $assemble_fn(&mut buf);
        let instructions = cs.disasm_all(&buf, 0).unwrap();
        assert_eq!(
            $format_fn(),
            merge_instructions_without_line_numbers(instructions)
        );
    }};
    ($assemble_fn: expr, $format_fn: expr, $iter:expr) => {{
        use $crate::generic64::disassembler_test_macro::merge_instructions_without_line_numbers;
        let arena = bumpalo::Bump::new();
        let (mut buf, cs) = setup_capstone_and_arena(&arena);
        for i in $iter.iter() {
            buf.clear();
            $assemble_fn(&mut buf, *i);
            let instructions = cs.disasm_all(&buf, 0).unwrap();
            assert_eq!(
                $format_fn(*i),
                merge_instructions_without_line_numbers(instructions)
            );
        }
    }};
    ($assemble_fn: expr, $format_fn: expr, $iter:expr, $iter2:expr) => {{
        use $crate::generic64::disassembler_test_macro::merge_instructions_without_line_numbers;
        let arena = bumpalo::Bump::new();
        let (mut buf, cs) = setup_capstone_and_arena(&arena);
        for i in $iter.iter() {
            for i2 in $iter2.iter() {
                buf.clear();
                $assemble_fn(&mut buf, *i, *i2);
                let instructions = cs.disasm_all(&buf, 0).unwrap();
                assert_eq!(
                    $format_fn(*i, *i2),
                    merge_instructions_without_line_numbers(instructions)
                );
            }
        }
    }};
    ($assemble_fn: expr, $format_fn: expr, $iter:expr, $iter2:expr, $iter3:expr) => {{
        use $crate::generic64::disassembler_test_macro::merge_instructions_without_line_numbers;
        let arena = bumpalo::Bump::new();
        let (mut buf, cs) = setup_capstone_and_arena(&arena);
        for i in $iter.iter() {
            for i2 in $iter2.iter() {
                for i3 in $iter3.iter() {
                    buf.clear();
                    $assemble_fn(&mut buf, *i, *i2, *i3);
                    let instructions = cs.disasm_all(&buf, 0).unwrap();
                    assert_eq!(
                        $format_fn(*i, *i2, *i3),
                        merge_instructions_without_line_numbers(instructions)
                    );
                }
            }
        }
    }};
    ($assemble_fn: expr, $format_fn: expr, $iter:expr, $iter2:expr, $iter3:expr, $iter4:expr) => {{
        use $crate::generic64::disassembler_test_macro::merge_instructions_without_line_numbers;
        let arena = bumpalo::Bump::new();
        let (mut buf, cs) = setup_capstone_and_arena(&arena);
        for i in $iter.iter() {
            for i2 in $iter2.iter() {
                for i3 in $iter3.iter() {
                    for i4 in $iter4.iter() {
                        buf.clear();
                        $assemble_fn(&mut buf, *i, *i2, *i3, *i4);
                        let instructions = cs.disasm_all(&buf, 0).unwrap();
                        assert_eq!(
                            $format_fn(*i, *i2, *i3, *i4),
                            merge_instructions_without_line_numbers(instructions)
                        );
                    }
                }
            }
        }
    }};
}
