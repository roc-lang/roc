use crate::bitmask::Bitmask;

#[derive(Default, Copy, Clone, PartialEq, Eq)]
pub struct Region {
    pub start_offset: u32,
    pub end_offset: u32,
}

impl Region {
    pub const ZERO: Self = Self {
        start_offset: 0,
        end_offset: 0,
    };
}

pub struct TokenMap {
    bitmask: Bitmask,
    // These don't *need* to be allocations, but seems unnecessary to risk stack overflows.
    // They're stored together in one allocation because they might as well be.
    tokens: [MaybeToken; 64],
    regions: [Region; 64],
}

impl TokenMap {
    pub fn clear(&mut self) {
        self.bitmask = Bitmask::ZERO;
        self.tokens = [MaybeToken::NONE; 64];
        self.regions = [Region::ZERO; 64];
    }

    pub fn iter(&self) -> TokensIter<'_> {
        TokensIter {
            bitmask: self.bitmask,
            token_map: self,
        }
    }

    pub fn set(&mut self, index: u8, maybe_token: MaybeToken, region: Region) {
        debug_assert!(index < 64);

        let cleared = self.bitmask & !(Bitmask::from(1) << index);
        let mask = Bitmask::from((maybe_token.is_some() as u64) << index);

        self.bitmask = cleared | mask;

        unsafe {
            *self.tokens.get_unchecked_mut(index as usize) = maybe_token;
            *self.regions.get_unchecked_mut(index as usize) = region;
        }
    }

    pub fn insert(&mut self, index: u8, token: Token, region: Region) {
        debug_assert!(index < 64);

        self.bitmask.set_to_1(index);

        unsafe {
            *self.tokens.get_unchecked_mut(index as usize) = token.into();
            *self.regions.get_unchecked_mut(index as usize) = region;
        }
    }

    pub fn remove(&mut self, index: u64) {
        debug_assert!(index < 64);

        self.bitmask.set_to_0(index);

        // We don't need to bother clearing the tokens or regions;
        // the only way to access them is through the bitmask, so
        // zeroing that bit is sufficient.
    }

    // pub fn bitmask(&self) -> Bitmask {
    //     self.bitmask
    // }

    // pub fn bitmask_mut(&mut self) -> &mut Bitmask {
    //     &mut self.bitmask
    // }

    // pub fn tokens(&self) -> &[MaybeToken; 64] {
    //     &self.tokens_and_regions.as_ref().0
    // }

    // pub fn tokens_mut(&mut self) -> &mut [MaybeToken; 64] {
    //     &mut self.tokens_and_regions.as_mut().0
    // }

    // pub fn regions(&self) -> &[Region; 64] {
    //     &self.tokens_and_regions.as_ref().1
    // }

    // pub fn regions_mut(&mut self) -> &mut [Region; 64] {
    //     &mut self.tokens_and_regions.as_mut().1
    // }
}

impl Default for TokenMap {
    fn default() -> Self {
        Self {
            bitmask: Default::default(),
            tokens: [MaybeToken::NONE; 64],
            regions: [Region::ZERO; 64],
        }
    }
}

/// Either a Token or a zero (which no Token variant uses).
/// This is basically an Option<Token> that fits in a u8,
/// because Rust doesn't currently optimize Option<Token>
/// to fit in a u8.
#[derive(Eq, Clone, Copy)]
pub union MaybeToken {
    raw: u8,
    token: Token,
}

impl PartialEq for MaybeToken {
    fn eq(&self, other: &Self) -> bool {
        unsafe { self.raw == other.raw }
    }
}

impl From<Token> for MaybeToken {
    fn from(token: Token) -> Self {
        MaybeToken { token }
    }
}

impl From<MaybeToken> for Option<Token> {
    fn from(maybe_token: MaybeToken) -> Self {
        if unsafe { maybe_token.raw } == 0 {
            None
        } else {
            Some(unsafe { maybe_token.token })
        }
    }
}

impl MaybeToken {
    pub const NONE: Self = Self { raw: 0 };

    pub const fn none() -> Self {
        Self::NONE
    }

    pub const fn some(token: Token) -> Self {
        Self { token }
    }

    fn is_some(self) -> bool {
        self != Self::NONE
    }
}

impl Default for MaybeToken {
    fn default() -> Self {
        Self::NONE
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum Token {
    // NOTE: this must never have a value of zero!

    // These numbers must all be the same as the corresponding numbers on InProgress
    // (except of course for the variants which this enum has, but InProgress doesn't have)
    Comment = 1,
    Lambda = 2,
    Indent = 3,
    Outdent = 4,
    InterpolationStart = 5,
    InterpolationEnd = 6,
    MultiLineStr = 7,
    SingleLineStr = 8,
    SingleQuoteChar = 9,
}

pub struct TokensIter<'a> {
    token_map: &'a TokenMap,
    bitmask: Bitmask,
}

impl<'a> Iterator for TokensIter<'a> {
    type Item = (Token, Region);

    fn next(&mut self) -> Option<Self::Item> {
        let bitmask = self.bitmask;

        if bitmask.is_nonzero() {
            let index = bitmask.trailing_zeros() as usize;

            self.bitmask &= bitmask.wrapping_sub(1); // clear the bit we just encountered

            // Safety: index comes from the bitmask, which has 64 entries; this must be less than 64.
            let token = unsafe { *self.token_map.tokens.get_unchecked(index) };
            let region = unsafe { *self.token_map.regions.get_unchecked(index) };

            let token = {
                debug_assert!(
                    token != MaybeToken::NONE,
                    "Encountered a MaybeToken::NONE at a bitmask index of 0"
                );

                // Safety: we enforce that whenever we insert a 1 into the bitmask,
                // it always refers to a valid Token (not a MaybeToken::NONE), so
                unsafe { std::mem::transmute::<MaybeToken, Token>(token) }
            };

            Some((token, region))
        } else {
            None
        }
    }
}

// impl Token {
//     unsafe fn from_non_nothing_in_progress(in_progress: InProgress) -> Self {
//         debug_assert_eq!(
//             match in_progress {
//                 InProgress::Nothing => unreachable!(),
//                 InProgress::Comment => Token::Comment as u8,
//                 InProgress::Lambda => Token::Lambda as u8,
//                 InProgress::MultiLineStr => Token::MultiLineStr as u8,
//                 InProgress::SingleLineStr => Token::SingleLineStr as u8,
//                 InProgress::SingleQuoteChar => Token::SingleLineStr as u8,
//             },
//             in_progress as u8
//         );

//         std::mem::transmute::<InProgress, Token>(in_progress)
//     }
// }
