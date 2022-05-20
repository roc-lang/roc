use crate::subs::{Content, Descriptor, Mark, OptVariable, Rank, Variable, VariableSubsSlice};

#[derive(Clone)]
pub struct UnificationTable {
    contents: Vec<Content>,
    ranks: Vec<Rank>,
    marks: Vec<Mark>,
    copies: Vec<OptVariable>,
    redirects: Vec<OptVariable>,
}

struct Snapshot(UnificationTable);

impl UnificationTable {
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            contents: Vec::with_capacity(cap),  // vec![Content::Error; cap],
            ranks: Vec::with_capacity(cap),     // vec![Rank::NONE; cap],
            marks: Vec::with_capacity(cap),     // vec![Mark::NONE; cap],
            copies: Vec::with_capacity(cap),    // vec![OptVariable::NONE; cap],
            redirects: Vec::with_capacity(cap), // vec![OptVariable::NONE; cap],
        }
    }

    pub fn len(&self) -> usize {
        self.contents.len()
    }

    pub fn is_empty(&self) -> bool {
        self.contents.is_empty()
    }

    pub fn reserve(&mut self, extra_length: usize) -> VariableSubsSlice {
        use std::iter::repeat;

        let start = self.contents.len();

        self.contents
            .extend(repeat(Content::Error).take(extra_length));
        self.ranks.extend(repeat(Rank::NONE).take(extra_length));
        self.marks.extend(repeat(Mark::NONE).take(extra_length));
        self.copies
            .extend(repeat(OptVariable::NONE).take(extra_length));
        self.redirects
            .extend(repeat(OptVariable::NONE).take(extra_length));

        VariableSubsSlice::new(start as _, extra_length as _)
    }

    pub fn push(&mut self, content: Content, rank: Rank, mark: Mark, copy: OptVariable) {
        self.contents.push(content);
        self.ranks.push(rank);
        self.marks.push(mark);
        self.copies.push(copy);
        self.redirects.push(OptVariable::NONE);
    }

    pub fn set(
        &mut self,
        key: Variable,
        content: Content,
        rank: Rank,
        mark: Mark,
        copy: OptVariable,
    ) {
        let index = self.root_key(key).index() as usize;

        self.contents[index] = content;
        self.ranks[index] = rank;
        self.marks[index] = mark;
        self.copies[index] = copy;
    }

    #[inline(always)]
    pub fn unify_right_to_left(&mut self, to: Variable, from: Variable) {
        self.redirects[to.index() as usize] = OptVariable::from(from);
    }

    #[inline(always)]
    pub fn root_key(&mut self, mut key: Variable) -> Variable {
        let index = key.index() as usize;

        while let Some(redirect) = self.redirects[key.index() as usize].into_variable() {
            key = redirect;
        }

        self.redirects[index] = OptVariable::from(key);

        key
    }

    #[inline(always)]
    pub fn root_key_without_compacting(&self, mut key: Variable) -> Variable {
        while let Some(redirect) = self.redirects[key.index() as usize].into_variable() {
            key = redirect;
        }

        key
    }

    #[inline(always)]
    pub fn get_rank(&self, key: Variable) -> Rank {
        self.ranks[self.root_key_without_compacting(key).index() as usize]
    }

    #[inline(always)]
    pub fn get_mark(&self, key: Variable) -> Mark {
        self.marks[self.root_key_without_compacting(key).index() as usize]
    }

    #[inline(always)]
    pub fn get_content(&self, key: Variable) -> &Content {
        &self.contents[self.root_key_without_compacting(key).index() as usize]
    }

    #[inline(always)]
    pub fn set_rank(&mut self, key: Variable, value: Rank) {
        let index = self.root_key(key).index() as usize;
        self.ranks[index] = value;
    }

    #[inline(always)]
    pub fn set_mark(&mut self, key: Variable, value: Mark) {
        let index = self.root_key(key).index() as usize;
        self.marks[index] = value;
    }

    #[inline(always)]
    pub fn set_content(&mut self, key: Variable, value: Content) {
        let index = self.root_key(key).index() as usize;
        self.contents[index] = value;
    }

    pub fn snapshot(&self) -> Snapshot {
        Snapshot(self.clone())
    }

    pub fn rollback_to(&mut self, snapshot: Snapshot) {
        *self = snapshot.0;
    }
}
