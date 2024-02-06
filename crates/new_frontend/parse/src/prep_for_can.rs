use arena::Arena;
use intern::InternKey;

/// Prepare for canonicalization by:
/// -

struct State<'a> {
    /// Each Pattern has a bit indicating whether the pattern continues.
    /// So we can look at each Pattern entry and tell whether the pattern is done.
    top_level_patterns: Vec16<'a, Pattern<'a>>,

    /// The idea is that it will be possible to tell when each of these exprs ends.
    /// At that point we proceed to the next entry in top_level_defs. There's no
    /// need to record a mapping between which expr corresponds to which top level def,
    /// because we only care about that in the course of traversing these, and in that
    /// situation we can already tell which expr corresponds to which top-level def.
    top_level_bodies: Vec16<'a, Expr<'a>>,

    /// TODO: In the future this will be more complex state, e.g. there can be a bunch
    /// of different things that we're in the middle of.
    top_level_pattern_in_progress: bool,
}

enum Token<'a> {
    OpenCurly,
    CloseCurly,
    Ident(&'a str),
}

struct Region {
    line: u16,
    col: u16,
}

impl<'a> State<'a> {
    pub fn receive_token(&mut self, arena: &mut Arena<'a>, token: Token, region: Region) {
        //
    }
}
