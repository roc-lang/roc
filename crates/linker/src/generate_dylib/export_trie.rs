///! Partial implementation of an Mach-O export trie encoder.
///!
///! Based on llvm-project/lld/MachO/ExportTrie.cpp
use std::io::{Cursor, Write};

use crate::pe::next_multiple_of;

/// Construct a macho export trie.
///
/// We take some shortcuts here. In particular, we default the address of a symbol to 0, because
/// this address is not (currently) used in our compilation pipeline.
pub(crate) fn build(symbols: &mut [&str]) -> Vec<u8> {
    let mut buffer = Cursor::new(Vec::new());

    let mut builder = TrieBuilder {
        nodes: vec![TrieNode::default()],
    };

    builder.sort_and_build(symbols, 0, 0, 0);

    loop {
        let mut offset = 0;
        let mut more = false;

        for node_index in 0..builder.nodes.len() {
            more |= builder.update_offset(node_index, &mut offset);
        }

        if !more {
            break;
        }
    }

    for node in &builder.nodes {
        builder.write_to(node, &mut buffer).unwrap();
    }

    let padding = next_multiple_of(buffer.get_ref().len(), 8) - buffer.get_ref().len();
    buffer.get_mut().extend(std::iter::repeat(0).take(padding));

    buffer.into_inner()
}

type TrieNodeIndex = usize;

#[derive(Debug)]
struct Edge {
    substring: String,
    child: TrieNodeIndex,
}

#[derive(Default, Debug)]
struct TrieNode {
    edges: Vec<Edge>,
    info: Option<ExportInfo>,

    // Estimated offset from the start of the serialized trie to the current node.
    // This will converge to the true offset when updateOffset() is run to a
    // fixpoint.
    offset: usize,
}

#[derive(Debug)]
struct ExportInfo {
    address: u64,
    flags: u64,
}

// this is a bit of a deviation from the C++ implementation, here so we don't have to worry about
// the borrow checker. Instead of references, we use indices into the `nodes` array to link nodes
// together.
#[derive(Debug)]
struct TrieBuilder {
    nodes: Vec<TrieNode>,
}

impl TrieBuilder {
    fn update_offset(&mut self, node_index: TrieNodeIndex, next_offset: &mut usize) -> bool {
        let node = &self.nodes[node_index];

        let mut node_size = if let Some(info) = &node.info {
            let terminal_size = get_uleb128_size(info.flags) + get_uleb128_size(info.address);

            // Overall node size so far is the uleb128 size of the length of the symbol
            // info + the symbol info itself.
            terminal_size + get_uleb128_size(terminal_size as u64)
        } else {
            1 // size of terminal_size (which has a value of 0)
        };

        // compute size of all child edges
        node_size += 1; // byte for number of children
        for edge in &node.edges {
            node_size +=
                edge.substring.len() + 1 + get_uleb128_size(self.nodes[edge.child].offset as u64);
        }

        let result = node.offset != *next_offset;

        self.nodes[node_index].offset = *next_offset;
        *next_offset += node_size;

        result
    }

    fn write_to(&self, node: &TrieNode, cursor: &mut Cursor<Vec<u8>>) -> std::io::Result<()> {
        let delta = node.offset.saturating_sub(cursor.get_ref().len());
        cursor.get_mut().extend(std::iter::repeat(0).take(delta));
        cursor.set_position(node.offset as u64);

        if let Some(info) = &node.info {
            let terminal_size = get_uleb128_size(info.flags) + get_uleb128_size(info.address);

            leb128::write::unsigned(cursor, terminal_size as u64)?;
            leb128::write::unsigned(cursor, info.flags as u64)?;
            leb128::write::unsigned(cursor, info.address as u64)?;
        } else {
            // TrieNode with no symbol info
            cursor.write_all(&[0])?; // terminal size
        }

        // add number of children

        // TODO handle case where we have more than 256
        assert!(node.edges.len() < 256);

        cursor.write_all(&[node.edges.len() as u8])?;

        for edge in &node.edges {
            cursor.write_all(edge.substring.as_bytes())?;
            cursor.write_all(&[0])?;

            leb128::write::unsigned(cursor, self.nodes[edge.child].offset as u64)?;
        }

        Ok(())
    }

    // Build the trie by performing a three-way radix quicksort: We start by sorting
    // the strings by their first characters, then sort the strings with the same
    // first characters by their second characters, and so on recursively. Each
    // time the prefixes diverge, we add a node to the trie.
    //
    // node:    The most recently created node along this path in the trie (i.e.
    //          the furthest from the root.)
    // lastPos: The prefix length of the most recently created node, i.e. the number
    //          of characters along its path from the root.
    // pos:     The string index we are currently sorting on. Note that each symbol
    //          S contained in vec has the same prefix S[0...pos).
    fn sort_and_build(
        &mut self,
        symbols: &mut [&str],
        mut node_index: TrieNodeIndex,
        mut last_pos: usize,
        pos: usize,
    ) {
        if symbols.is_empty() {
            return;
        }

        // Partition items so that items in [0, i) are less than the pivot,
        // [i, j) are the same as the pivot, and [j, vec.size()) are greater than
        // the pivot.
        let pivot_symbol = symbols[symbols.len() / 2];
        let pivot = pivot_symbol.as_bytes().get(pos);

        let mut i = 0;
        let mut j = symbols.len();

        let mut k = 0;
        while k < j {
            let c = symbols[k].as_bytes().get(pos);
            match c.cmp(&pivot) {
                std::cmp::Ordering::Less => {
                    symbols.swap(i, k);
                    i += 1;
                    k += 1;
                }
                std::cmp::Ordering::Equal => {
                    k += 1;
                }
                std::cmp::Ordering::Greater => {
                    j -= 1;
                    symbols.swap(j, k)
                }
            }
        }

        let is_terminal = pivot.is_none();
        let prefixes_diverge = i != 0 || j != symbols.len();

        if last_pos != pos && (is_terminal || prefixes_diverge) {
            let new_node = TrieNode {
                edges: vec![],
                info: None,
                offset: 0,
            };

            let new_node_index = self.nodes.len();
            self.nodes.push(new_node);

            let edge = Edge {
                substring: pivot_symbol[last_pos..pos].to_string(),
                child: new_node_index,
            };

            self.nodes[node_index].edges.push(edge);

            node_index = new_node_index;
            last_pos = pos;
        }

        self.sort_and_build(&mut symbols[0..i], node_index, last_pos, pos);
        self.sort_and_build(&mut symbols[j..], node_index, last_pos, pos);

        if is_terminal {
            assert!(j - i == 1); // no duplicate symbols
            self.nodes[node_index].info = Some(ExportInfo {
                address: 0,
                flags: 0,
            });
        } else {
            // This is the tail-call-optimized version of the following:
            if j.saturating_sub(i) > i {
                self.sort_and_build(&mut symbols[i..j - i], node_index, last_pos, pos + 1);
            }
        }
    }
}

fn get_uleb128_size(val: u64) -> usize {
    let mut scratchpad = std::io::Cursor::new([0; 16]);
    leb128::write::unsigned(&mut scratchpad, val).unwrap()
}
