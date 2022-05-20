use crate::subs::{Content, Descriptor, Mark, OptVariable, Rank, Variable, VariableSubsSlice};

#[derive(Clone, Default)]
pub struct UnificationTable {
    contents: Vec<Content>,
    ranks: Vec<Rank>,
    marks: Vec<Mark>,
    copies: Vec<OptVariable>,
    redirects: Vec<OptVariable>,
}

pub struct Snapshot(UnificationTable);

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
            .extend(repeat(Content::FlexVar(None)).take(extra_length));
        self.ranks.extend(repeat(Rank::NONE).take(extra_length));
        self.marks.extend(repeat(Mark::NONE).take(extra_length));
        self.copies
            .extend(repeat(OptVariable::NONE).take(extra_length));
        self.redirects
            .extend(repeat(OptVariable::NONE).take(extra_length));

        VariableSubsSlice::new(start as _, extra_length as _)
    }

    pub fn push(
        &mut self,
        content: Content,
        rank: Rank,
        mark: Mark,
        copy: OptVariable,
    ) -> Variable {
        let variable = unsafe { Variable::from_index(self.len() as _) };

        self.contents.push(content);
        self.ranks.push(rank);
        self.marks.push(mark);
        self.copies.push(copy);
        self.redirects.push(OptVariable::NONE);

        variable
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
    pub fn root_key(&mut self, mut key: Variable) -> Variable {
        let index = key.index() as usize;

        while let Some(redirect) = self.redirects[key.index() as usize].into_variable() {
            key = redirect;
        }

        if index != key.index() as usize {
            self.redirects[index] = OptVariable::from(key);
        }

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
    pub fn get_copy(&self, key: Variable) -> OptVariable {
        let index = self.root_key_without_compacting(key).index() as usize;
        self.copies[index]
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
    pub fn set_copy(&mut self, key: Variable, value: OptVariable) {
        let index = self.root_key(key).index() as usize;
        self.copies[index] = value;
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

    pub fn vars_since_snapshot(&self, snapshot: &Snapshot) -> std::ops::Range<Variable> {
        unsafe {
            let start = Variable::from_index(snapshot.0.len() as u32);
            let end = Variable::from_index(self.len() as u32);

            start..end
        }
    }

    pub fn is_redirect(&self, key: Variable) -> bool {
        self.redirects[key.index() as usize].is_some()
    }

    pub fn unioned(&mut self, a: Variable, b: Variable) -> bool {
        self.root_key(a) == self.root_key(b)
    }

    // TODO remove
    #[inline(always)]
    pub fn inlined_get_root_key(&mut self, key: Variable) -> Variable {
        self.root_key(key)
    }

    /// NOTE: assumes variables are root
    pub fn unify_roots(&mut self, to: Variable, from: Variable, desc: Descriptor) {
        let from_index = from.index() as usize;
        let to_index = to.index() as usize;

        // redirect from -> to
        if from_index != to_index {
            self.redirects[from_index] = OptVariable::from(to);
        }

        // update to's Descriptor
        self.contents[to_index] = desc.content;
        self.ranks[to_index] = desc.rank;
        self.marks[to_index] = desc.mark;
        self.copies[to_index] = desc.copy;
    }

    pub fn get_descriptor(&self, key: Variable) -> Descriptor {
        let index = self.root_key_without_compacting(key).index() as usize;

        Descriptor {
            content: self.contents[index],
            rank: self.ranks[index],
            mark: self.marks[index],
            copy: self.copies[index],
        }
    }

    pub fn set_descriptor(&mut self, key: Variable, desc: Descriptor) {
        let index = self.root_key(key).index() as usize;

        self.contents[index] = desc.content;
        self.ranks[index] = desc.rank;
        self.marks[index] = desc.mark;
        self.copies[index] = desc.copy;
    }
}
