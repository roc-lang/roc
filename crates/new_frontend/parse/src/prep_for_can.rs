use arena::Arena;
use intern::InternKey;

/// Prepare for canonicalization by:
/// -

struct State<'t, 'd, 'r> {
    /// Arena used for temporary allocations. This will be reset once we've moved to the next phase.
    tmp: &'t mut Arena<'t>,

    /// Arena used for recording things that last all the way until the reporting step of compilation
    /// (e.g. interns). In batch builds, these will never be reset. In watch builds, they will only be
    /// reset in between watch builds.
    forever: &'r mut Arena<'r>,

    /// Arena used for other durable allocations. Anything allocated in here will live through multiple
    /// compiler phases.
    ///
    /// We can see how much of a difference this makes in the future by using the same arena for
    /// both the temporary and durable allocators, and then removing the resets. If compile times
    /// don't get noticeably worse, then it didn't help. But it should save page faults, which
    /// should be relevant.
    dur: &'d mut Arena<'d>,

    /// Each of these that's a Pattern has a bit indicating whether the pattern continues.
    /// So we can look at each Pattern entry and tell whether the pattern is done.
    /// (When we encounter a token that tells us the pattern has ended, we can go back
    /// and mutate the previous node to set the bit indicating it's the end of the Pattern.)
    /// Similarly, each of these that's an Annotation has such a bit as well.
    /// Using these combined, plus looking at line numbers, we can tell which annotations
    /// were in fact immediately preceding patterns. Also I think there's a reasonable argument
    /// for a design change here, where we just say "as long as the annotation is followed by a
    /// matching pattern, it goes with it" and then the formatter collapses them. Yeah that
    /// seems like a better design.
    nodes: Vec32<'t, Node<'t>>,

    /// As we encounter semantic strings (variables, field names, tag names, type/ability/etc names)
    /// we intern them in here. String literals just get saved as a byte range into the source file.
    /// These will be preserved all the way to the end of compilation, because they get used in reporting.
    interns: Scope<'r>,

    /// The initial scope we're going to prepare for canonicalization. Bas
    scope: Scope<'d>,

    /// Which top-level defs reference each other. We need this info in order to reorder them.
    /// Note that this has to be a growable vec; we may need to resize it as we encounter more
    /// top-level defs. And since it's n^2 bits, we have to be careful not to preallocate too
    /// much capacity.
    top_level_references: Vec32<'t, bool>,

    /// The index in nodes where each top-level def begins. In conjunction
    /// with top_level_references, we can change the order of this vec and then canonicalization
    /// can traverse this vec in order to canonicalize the top-level defs in the correct order.
    top_level_def_offsets: Vec16<'d, u32>,
}

struct ReferenceMatrix {}

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
