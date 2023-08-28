use bitflags::bitflags;

bitflags! {
    pub struct UnificationMode : u8 {
        /// Instructs the unifier to solve two types for equality.
        ///
        /// For example, { n : Str }a ~ { n: Str, m : Str } will solve "a" to "{ m : Str }".
        const EQ = 1 << 0;
        /// Instructs the unifier to treat the right-hand-side of a constraint as
        /// present in the left-hand-side, rather than strictly equal.
        ///
        /// For example, t1 += [A Str] says we should "add" the tag "A Str" to the type of "t1".
        const PRESENT = 1 << 1;
        /// Like [`UnificationMode::EQ`], but also instructs the unifier that the ambient lambda set
        /// specialization algorithm is running. This has implications for the unification of
        /// unspecialized lambda sets; see [`unify_unspecialized_lambdas`].
        const LAMBDA_SET_SPECIALIZATION = UnificationMode::EQ.bits | (1 << 2);
    }
}

impl UnificationMode {
    pub fn is_eq(&self) -> bool {
        debug_assert!(!self.contains(UnificationMode::EQ | UnificationMode::PRESENT));
        self.contains(UnificationMode::EQ)
    }

    pub fn is_present(&self) -> bool {
        debug_assert!(!self.contains(UnificationMode::EQ | UnificationMode::PRESENT));
        self.contains(UnificationMode::PRESENT)
    }

    pub fn is_lambda_set_specialization(&self) -> bool {
        debug_assert!(!self.contains(UnificationMode::EQ | UnificationMode::PRESENT));
        self.contains(UnificationMode::LAMBDA_SET_SPECIALIZATION)
    }

    pub fn as_eq(self) -> Self {
        (self - UnificationMode::PRESENT) | UnificationMode::EQ
    }

    pub fn pretty_print(&self) -> &str {
        if self.contains(UnificationMode::EQ) {
            "~"
        } else if self.contains(UnificationMode::PRESENT) {
            "+="
        } else {
            unreachable!("Bad mode!")
        }
    }
}
