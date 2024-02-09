pub type ConstId = u16;
pub type TypeId = u16;

pub struct State<'a> {
    lowercase: Interns<'a>,
    uppercase: Interns<'a>,

    pattern_names_by_id: Vec32<'a, InternKey>,
    regions_by_id: Vec32<'a, InternKey>,
    is_fn_by_id: Vec32<'a, bool>,

    // We record this after we've done the prep (which adds all the top-level defs).
    // This number therefore tells us that the first N entries in the by_id vecs are
    // top-level declarations, which in turn means that any ConstId that's less than
    // this number is a top-level declaration, and all the others aren't.
    num_top_level_patterns: u16,

    next_const_id: ConstId,

    /// We don't bother recording type annotations because they just get turned into constraints
    constraints: Vec32<'a, Constraint>
}

impl<'a> State<'a> {
    /// These are the arguments to Self::from_prepared
    pub struct Prepared<'a> {
        pub lowercase: Interns<'a>,
        pub uppercase: Interns<'a>,

        pub pattern_names_by_id: Vec32<'a, InternKey>,
        pub regions_by_id: Vec32<'a, InternKey>,
        pub is_fn_by_id: Vec32<'a, bool>,

        pub annotations_by_id: Vec32<'a, Option<Ann>>,
    }

    pub fn from_prepared(prep: Prepared<'a>) -> Self {
        let num_top_level_patterns = prep.pattern_names_by_id.len();

        Self {
            lowercase,
            uppercase,

            pattern_names_by_id,
            regions_by_id,
            is_fn_by_id,

            num_top_level_patterns,
            next_const_id: num_top_level_patterns,
        }
    }

    pub fn is_tl_const(&self, id: ConstId) -> bool {
        // We recorded the number of top-level patterns from the previous step,
        // so now we know everything added after that must not be top-level.
        id < self.num_top_level_patterns
    }
}
