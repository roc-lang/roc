use std::hint::unreachable_unchecked;

use crate::subs::{Content, Descriptor, Mark, OptVariable, Rank, Variable, VariableSubsSlice};

#[derive(Clone, Default)]
pub struct UnificationTable {
    contents: Vec<Content>,
    metadata: Vec<Combine>,
}

pub struct Snapshot(UnificationTable);

#[derive(Debug, Clone, Copy)]
enum Combine {
    Redirect(Variable),
    Root(Root),
}

#[derive(Debug, Clone, Copy)]
pub struct Root {
    rank: Rank,
    mark: Mark,
    copy: OptVariable,
}

impl Root {
    const NONE: Self = Self {
        rank: Rank::NONE,
        mark: Mark::NONE,
        copy: OptVariable::NONE,
    };
}

impl Combine {
    const NONE: Self = Self::Root(Root::NONE);
}

impl UnificationTable {
    #[allow(unused)]
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            contents: Vec::with_capacity(cap),
            metadata: Vec::with_capacity(cap),
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
        self.metadata
            .extend(repeat(Combine::NONE).take(extra_length));

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

        let combine = Combine::Root(Root { rank, mark, copy });

        self.metadata.push(combine);

        variable
    }

    #[allow(unused)]
    pub fn set(
        &mut self,
        key: Variable,
        content: Content,
        rank: Rank,
        mark: Mark,
        copy: OptVariable,
    ) {
        let root = self.root_key(key);
        self.set_unchecked(root, content, rank, mark, copy)
    }

    pub fn set_unchecked(
        &mut self,
        key: Variable,
        content: Content,
        rank: Rank,
        mark: Mark,
        copy: OptVariable,
    ) {
        let index = key.index() as usize;

        self.contents[index] = content;

        self.metadata[index] = Combine::Root(Root { rank, mark, copy });
    }

    pub fn modify<F, T>(&mut self, key: Variable, mapper: F) -> T
    where
        F: FnOnce(&mut Descriptor) -> T,
    {
        let root_key = self.root_key(key);
        let index = root_key.index() as usize;

        let root = self.get_root_unchecked(root_key);

        let mut desc = Descriptor {
            content: self.contents[index],
            rank: root.rank,
            mark: root.mark,
            copy: root.copy,
        };

        let result = mapper(&mut desc);

        self.set_unchecked(root_key, desc.content, desc.rank, desc.mark, desc.copy);

        result
    }

    // GET UNCHECKED

    #[inline(always)]
    pub fn get_root_unchecked(&self, key: Variable) -> Root {
        match self.metadata[key.index() as usize] {
            Combine::Root(root) => root,
            Combine::Redirect(_) => {
                if cfg!(debug_assertions) {
                    unreachable!("unchecked access on non-root variable {:?}", key);
                } else {
                    unsafe { unreachable_unchecked() }
                }
            }
        }
    }

    #[inline(always)]
    pub fn get_rank_unchecked(&self, key: Variable) -> Rank {
        self.get_root_unchecked(key).rank
    }

    #[inline(always)]
    pub fn get_mark_unchecked(&self, key: Variable) -> Mark {
        self.get_root_unchecked(key).mark
    }

    #[allow(unused)]
    #[inline(always)]
    pub fn get_copy_unchecked(&self, key: Variable) -> OptVariable {
        self.get_root_unchecked(key).copy
    }

    #[inline(always)]
    pub fn get_content_unchecked(&self, key: Variable) -> &Content {
        &self.contents[key.index() as usize]
    }

    // GET CHECKED

    #[inline(always)]
    pub fn get_root(&self, key: Variable) -> Root {
        let root_key = self.root_key_without_compacting(key);
        match self.metadata[root_key.index() as usize] {
            Combine::Root(root) => root,
            Combine::Redirect(_) => {
                if cfg!(debug_assertions) {
                    unreachable!("the root key {:?} is not actually a root key", root_key);
                } else {
                    unsafe { unreachable_unchecked() }
                }
            }
        }
    }

    #[inline(always)]
    pub fn get_rank(&self, key: Variable) -> Rank {
        self.get_root(key).rank
    }

    #[inline(always)]
    pub fn get_mark(&self, key: Variable) -> Mark {
        self.get_root(key).mark
    }

    #[inline(always)]
    pub fn get_copy(&self, key: Variable) -> OptVariable {
        self.get_root(key).copy
    }

    #[inline(always)]
    pub fn get_content(&self, key: Variable) -> &Content {
        &self.contents[self.root_key_without_compacting(key).index() as usize]
    }

    // SET UNCHECKED

    #[inline(always)]
    pub fn modify_root_unchecked<F, T>(&mut self, key: Variable, f: F) -> T
    where
        F: Fn(&mut Root) -> T,
    {
        match &mut self.metadata[key.index() as usize] {
            Combine::Root(root) => f(root),
            Combine::Redirect(_) => {
                if cfg!(debug_assertions) {
                    unreachable!("unchecked access on non-root variable {:?}", key);
                } else {
                    unsafe { unreachable_unchecked() }
                }
            }
        }
    }

    #[inline(always)]
    pub fn set_rank_unchecked(&mut self, key: Variable, value: Rank) {
        self.modify_root_unchecked(key, |root| root.rank = value)
    }

    #[inline(always)]
    pub fn set_mark_unchecked(&mut self, key: Variable, value: Mark) {
        self.modify_root_unchecked(key, |root| root.mark = value)
    }

    #[allow(unused)]
    #[inline(always)]
    pub fn set_copy_unchecked(&mut self, key: Variable, value: OptVariable) {
        self.modify_root_unchecked(key, |root| root.copy = value)
    }

    #[allow(unused)]
    #[inline(always)]
    pub fn set_content_unchecked(&mut self, key: Variable, value: Content) {
        self.contents[key.index() as usize] = value;
    }

    // SET CHECKED

    #[inline(always)]
    pub fn set_rank(&mut self, key: Variable, value: Rank) {
        let root_key = self.root_key(key);
        self.modify_root_unchecked(root_key, |root| root.rank = value)
    }

    #[inline(always)]
    pub fn set_mark(&mut self, key: Variable, value: Mark) {
        let root_key = self.root_key(key);
        self.modify_root_unchecked(root_key, |root| root.mark = value)
    }

    #[inline(always)]
    pub fn set_copy(&mut self, key: Variable, value: OptVariable) {
        let root_key = self.root_key(key);
        self.modify_root_unchecked(root_key, |root| root.copy = value)
    }

    #[inline(always)]
    pub fn set_content(&mut self, key: Variable, value: Content) {
        let index = self.root_key(key).index() as usize;
        self.contents[index] = value;
    }

    // ROOT KEY

    #[inline(always)]
    pub fn root_key(&mut self, mut key: Variable) -> Variable {
        let root = self.root_key_without_compacting(key);

        while let Combine::Redirect(redirect) = &mut self.metadata[key.index() as usize] {
            key = *redirect;
            *redirect = root;
        }

        root
    }

    #[inline(always)]
    pub fn root_key_without_compacting(&self, mut key: Variable) -> Variable {
        while let Combine::Redirect(redirect) = self.metadata[key.index() as usize] {
            key = redirect;
        }

        key
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
        matches!(self.metadata[key.index() as usize], Combine::Redirect(_))
    }

    pub fn unioned(&mut self, a: Variable, b: Variable) -> bool {
        self.root_key(a) == self.root_key(b)
    }

    // custom very specific helpers
    #[inline(always)]
    pub fn get_rank_set_mark(&mut self, key: Variable, mark: Mark) -> Rank {
        let root_key = self.root_key(key);

        self.modify_root_unchecked(root_key, |root| {
            root.mark = mark;

            root.rank
        })
    }

    /// NOTE: assumes variables are root
    pub fn unify_roots(&mut self, to: Variable, from: Variable, desc: Descriptor) {
        let from_index = from.index() as usize;
        let to_index = to.index() as usize;

        // redirect from -> to
        if from_index != to_index {
            self.metadata[from_index] = Combine::Redirect(to)
        }

        // update to's Descriptor
        self.set_unchecked(to, desc.content, desc.rank, desc.mark, desc.copy);
    }

    pub fn get_descriptor(&self, key: Variable) -> Descriptor {
        let root_key = self.root_key_without_compacting(key);
        let index = root_key.index() as usize;

        let root = self.get_root_unchecked(root_key);

        Descriptor {
            content: self.contents[index],
            rank: root.rank,
            mark: root.mark,
            copy: root.copy,
        }
    }

    pub fn set_descriptor(&mut self, key: Variable, desc: Descriptor) {
        self.set(key, desc.content, desc.rank, desc.mark, desc.copy);
    }

    pub(crate) fn serialize(
        &self,
        writer: &mut impl std::io::Write,
        mut written: usize,
    ) -> std::io::Result<usize> {
        use crate::subs::Subs;

        written = Subs::serialize_slice(&self.contents, writer, written)?;

        let mut ranks = Vec::new();
        let mut marks = Vec::new();
        let mut copies = Vec::new();
        let mut redirects = Vec::new();

        for c in self.metadata.iter() {
            match c {
                Combine::Redirect(redirect) => {
                    let root = Root::NONE;

                    ranks.push(root.rank);
                    marks.push(root.mark);
                    copies.push(root.copy);

                    redirects.push(OptVariable::from(*redirect));
                }
                Combine::Root(root) => {
                    ranks.push(root.rank);
                    marks.push(root.mark);
                    copies.push(root.copy);

                    redirects.push(OptVariable::NONE);
                }
            }
        }

        written = Subs::serialize_slice(&ranks, writer, written)?;
        written = Subs::serialize_slice(&marks, writer, written)?;
        written = Subs::serialize_slice(&copies, writer, written)?;
        written = Subs::serialize_slice(&redirects, writer, written)?;

        Ok(written)
    }

    pub(crate) fn deserialize(bytes: &[u8], length: usize, offset: usize) -> (Self, usize) {
        use crate::subs::Subs;

        let (contents, offset) = Subs::deserialize_slice::<Content>(bytes, length, offset);
        let (ranks, offset) = Subs::deserialize_slice::<Rank>(bytes, length, offset);
        let (marks, offset) = Subs::deserialize_slice::<Mark>(bytes, length, offset);
        let (copies, offset) = Subs::deserialize_slice::<OptVariable>(bytes, length, offset);
        let (redirects, offset) = Subs::deserialize_slice::<OptVariable>(bytes, length, offset);

        let mut metadata = Vec::with_capacity(ranks.len());

        let it = ranks
            .iter()
            .zip(marks.iter())
            .zip(copies.iter())
            .zip(redirects.iter());
        for (((rank, mark), copy), redirect) in it {
            match redirect.into_variable() {
                Some(redirect) => metadata.push(Combine::Redirect(redirect)),
                None => {
                    metadata.push(Combine::Root(Root {
                        rank: *rank,
                        mark: *mark,
                        copy: *copy,
                    }));
                }
            }
        }

        let this = Self {
            contents: contents.to_vec(),
            metadata,
        };

        (this, offset)
    }
}
