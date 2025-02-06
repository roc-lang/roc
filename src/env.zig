// An environment containing indexable data useful throughout most or all stages
// in the new compiler pipeline.
// pub const Env = struct {
//     pub symbols: SymbolStore,
//     // no deduping because these tend to be unique and potentially large
//     string_literals: Vec<String>,
//     tag_names: TagNameCache,
//     field_names: FieldNameCache,
//     problems: Vec<Problem>,
//     // TODO: these should probably be made a part of `problems`
//     compiler_problems: Vec<CompilerProblem>,
//     // TODO: where are these used, and how do we manage them?
//     // pub tuple_elem_indices: Vec<usize>,
//     // pub record_fields: Vec<RecordField<()>>,

//     pub fn add_string_literal(&mut self, s: String) -> StringLiteralId {
//         let len = self.string_literals.len();
//         self.string_literals.push(s);

//         StringLiteralId(Index::new(len as u32))
//     }

//     pub fn add_field_name(&mut self, name: &str) -> FieldNameId {
//         self.field_names.add_name(name)
//     }

//     pub fn add_field_name_slice(
//         &mut self,
//         name_ids: impl IntoIterator<Item = FieldNameId>,
//     ) -> FieldNameIdSlice {
//         self.field_names.add_name_slice(name_ids)
//     }

//     pub fn add_tag_name(&mut self, name: &str) -> TagNameId {
//         self.tag_names.add_name(name)
//     }

//     pub fn add_tag_name_slice(
//         &mut self,
//         name_ids: impl IntoIterator<Item = TagNameId>,
//     ) -> TagNameIdSlice {
//         self.tag_names.add_name_slice(name_ids)
//     }
// }

// pub struct StringLiteralId(Index<String>);

// impl core::ops::Index<StringLiteralId> for Env {
//     type Output = str;

//     fn index(&self, index: StringLiteralId) -> &Self::Output {
//         &self.string_literals[index.0.index()]
//     }
// }

// pub struct FieldNameId(DedupedStringId);

// impl core::ops::Index<FieldNameId> for Env {
//     type Output = str;

//     fn index(&self, index: FieldNameId) -> &Self::Output {
//         &self.field_names.field_names[index.0]
//     }
// }

// pub struct FieldNameIdSlice(Slice<FieldNameId>);

// impl core::ops::Index<FieldNameIdSlice> for Env {
//     type Output = [FieldNameId];

//     fn index(&self, index: FieldNameIdSlice) -> &Self::Output {
//         &self.field_names.name_ids_for_slicing[index.0.indices()]
//     }
// }

// #[derive(Debug, Default)]
// pub struct FieldNameCache {
//     field_names: DedupedStringStore,
//     name_ids_for_slicing: Vec<FieldNameId>,
// }

// impl FieldNameCache {
//     pub fn add_name(&mut self, name: &str) -> FieldNameId {
//         FieldNameId(self.field_names.insert(name))
//     }

//     pub fn add_name_slice(
//         &mut self,
//         name_ids: impl IntoIterator<Item = FieldNameId>,
//     ) -> FieldNameIdSlice {
//         FieldNameIdSlice(slice_extend_new(&mut self.name_ids_for_slicing, name_ids))
//     }
// }

// #[derive(Debug, Clone, Copy, PartialEq, Eq)]
// pub struct TagNameId(DedupedStringId);

// impl core::ops::Index<TagNameId> for Env {
//     type Output = str;

//     fn index(&self, index: TagNameId) -> &Self::Output {
//         &self.tag_names.tag_names[index.0]
//     }
// }

// #[derive(Debug, Clone, Copy, PartialEq, Eq)]
// pub struct TagNameIdSlice(Slice<TagNameId>);

// impl core::ops::Index<TagNameIdSlice> for Env {
//     type Output = [TagNameId];

//     fn index(&self, index: TagNameIdSlice) -> &Self::Output {
//         &self.tag_names.name_ids_for_slicing[index.0.indices()]
//     }
// }

// #[derive(Debug, Default)]
// pub struct TagNameCache {
//     tag_names: DedupedStringStore,
//     name_ids_for_slicing: Vec<TagNameId>,
// }

// impl TagNameCache {
//     pub fn add_name(&mut self, name: &str) -> TagNameId {
//         TagNameId(self.tag_names.insert(name))
//     }

//     pub fn add_name_slice(
//         &mut self,
//         name_ids: impl IntoIterator<Item = TagNameId>,
//     ) -> TagNameIdSlice {
//         TagNameIdSlice(slice_extend_new(&mut self.name_ids_for_slicing, name_ids))
//     }
// }
