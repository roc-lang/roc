#[macro_use]
extern crate pretty_assertions;

#[macro_use]
extern crate indoc;

mod helpers;

#[cfg(test)]
mod test_gen_rs {
    use crate::helpers::generate_bindings;

    #[test]
    fn record_aliased() {
        let module = indoc!(
            r#"
            MyRcd : { a : U64, b : I128 }

            main : MyRcd
            main = { a: 1u64, b: 2i128 }
        "#
        );

        assert_eq!(
            generate_bindings(module)
                .strip_prefix('\n')
                .unwrap_or_default(),
            indoc!(
                r#"
                #[derive(Clone, Copy, Debug, Default, Eq, Ord, Hash, PartialEq, PartialOrd)]
                #[repr(C)]
                pub struct MyRcd {
                    pub b: roc_std::I128,
                    pub a: u64,
                }
            "#
            )
        );
    }

    #[test]
    fn nested_record_aliased() {
        let module = indoc!(
            r#"
            Outer : { x : Inner, y : Str, z : List U8 }

            Inner : { a : U16, b : F32 }

            main : Outer
            main = { x: { a: 5, b: 24 }, y: "foo", z: [ 1, 2 ] }
        "#
        );

        assert_eq!(
            generate_bindings(module)
                .strip_prefix('\n')
                .unwrap_or_default(),
            indoc!(
                r#"
                #[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
                #[repr(C)]
                pub struct Outer {
                    pub y: roc_std::RocStr,
                    pub z: roc_std::RocList<u8>,
                    pub x: Inner,
                }

                #[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd)]
                #[repr(C)]
                pub struct Inner {
                    pub b: f32,
                    pub a: u16,
                }
            "#
            )
        );
    }

    #[test]
    fn record_anonymous() {
        let module = "main = { a: 1u64, b: 2u128 }";

        assert_eq!(
            generate_bindings(module)
                .strip_prefix('\n')
                .unwrap_or_default(),
            indoc!(
                r#"
                #[derive(Clone, Copy, Debug, Default, Eq, Ord, Hash, PartialEq, PartialOrd)]
                #[repr(C)]
                pub struct R1 {
                    pub b: roc_std::U128,
                    pub a: u64,
                }
            "#
            )
        );
    }

    #[test]
    fn nested_record_anonymous() {
        let module = r#"main = { x: { a: 5u16, b: 24f32 }, y: "foo", z: [ 1u8, 2 ] }"#;

        assert_eq!(
            generate_bindings(module)
                .strip_prefix('\n')
                .unwrap_or_default(),
            indoc!(
                r#"
                #[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
                #[repr(C)]
                pub struct R1 {
                    pub y: roc_std::RocStr,
                    pub z: roc_std::RocList<u8>,
                    pub x: R2,
                }

                #[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd)]
                #[repr(C)]
                pub struct R2 {
                    pub b: f32,
                    pub a: u16,
                }
            "#
            )
        );
    }

    #[test]
    fn tag_union_aliased() {
        let module = indoc!(
            r#"
            NonRecursive : [ Foo Str, Bar U128, Blah I32, Baz ]

            main : NonRecursive
            main = Foo "blah"
        "#
        );

        assert_eq!(
            generate_bindings(module)
                .strip_prefix('\n')
                .unwrap_or_default(),
            indoc!(
                r#"
                #[derive(Clone, Copy, Eq, Ord, Hash, PartialEq, PartialOrd)]
                #[repr(u8)]
                pub enum tag_NonRecursive {
                    Bar = 0,
                    Baz = 1,
                    Blah = 2,
                    Foo = 3,
                }

                impl core::fmt::Debug for tag_NonRecursive {
                    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                        match self {
                            Self::Bar => f.write_str("tag_NonRecursive::Bar"),
                            Self::Baz => f.write_str("tag_NonRecursive::Baz"),
                            Self::Blah => f.write_str("tag_NonRecursive::Blah"),
                            Self::Foo => f.write_str("tag_NonRecursive::Foo"),
                        }
                    }
                }

                #[repr(C)]
                pub union NonRecursive {
                    Bar: roc_std::U128,
                    Blah: i32,
                    Foo: core::mem::ManuallyDrop<roc_std::RocStr>,
                    _size_with_discriminant: [u8; 32],
                }

                impl NonRecursive {
                    pub fn tag(&self) -> tag_NonRecursive {
                        unsafe {
                            let bytes = core::mem::transmute::<&Self, &[u8; core::mem::size_of::<Self>()]>(self);

                            core::mem::transmute::<u8, tag_NonRecursive>(*bytes.as_ptr().add(24))
                        }
                    }

                    /// Internal helper
                    fn set_discriminant(&mut self, tag: tag_NonRecursive) {
                        let discriminant_ptr: *mut tag_NonRecursive = (self as *mut NonRecursive).cast();

                        unsafe {
                            *(discriminant_ptr.add(24)) = tag;
                        }
                    }

                    /// Construct a tag named Bar, with the appropriate payload
                    pub fn Bar(payload: roc_std::U128) -> Self {
                        let mut answer = Self {
                            Bar: payload
                        };

                        answer.set_discriminant(tag_NonRecursive::Bar);

                        answer
                    }

                    /// Unsafely assume the given NonRecursive has a .tag() of Bar and convert it to Bar's payload.
                    /// (Always examine .tag() first to make sure this is the correct variant!)
                    /// Panics in debug builds if the .tag() doesn't return Bar.
                    pub unsafe fn into_Bar(self) -> roc_std::U128 {
                        debug_assert_eq!(self.tag(), tag_NonRecursive::Bar);
                        self.Bar
                    }

                    /// Unsafely assume the given NonRecursive has a .tag() of Bar and return its payload.
                    /// (Always examine .tag() first to make sure this is the correct variant!)
                    /// Panics in debug builds if the .tag() doesn't return Bar.
                    pub unsafe fn as_Bar(&self) -> roc_std::U128 {
                        debug_assert_eq!(self.tag(), tag_NonRecursive::Bar);
                        self.Bar
                    }

                    /// A tag named Baz, which has no payload.
                    pub const Baz: Self = unsafe {
                        let mut bytes = [0; core::mem::size_of::<NonRecursive>()];

                        bytes[24] = tag_NonRecursive::Baz as u8;

                        core::mem::transmute::<[u8; core::mem::size_of::<NonRecursive>()], NonRecursive>(bytes)
                    };

                    /// Other `into_` methods return a payload, but since the Baz tag
                    /// has no payload, this does nothing and is only here for completeness.
                    pub fn into_Baz(self) {
                        ()
                    }

                    /// Other `as` methods return a payload, but since the Baz tag
                    /// has no payload, this does nothing and is only here for completeness.
                    pub unsafe fn as_Baz(&self) {
                        ()
                    }

                    /// Construct a tag named Blah, with the appropriate payload
                    pub fn Blah(payload: i32) -> Self {
                        let mut answer = Self {
                            Blah: payload
                        };

                        answer.set_discriminant(tag_NonRecursive::Blah);

                        answer
                    }

                    /// Unsafely assume the given NonRecursive has a .tag() of Blah and convert it to Blah's payload.
                    /// (Always examine .tag() first to make sure this is the correct variant!)
                    /// Panics in debug builds if the .tag() doesn't return Blah.
                    pub unsafe fn into_Blah(self) -> i32 {
                        debug_assert_eq!(self.tag(), tag_NonRecursive::Blah);
                        self.Blah
                    }

                    /// Unsafely assume the given NonRecursive has a .tag() of Blah and return its payload.
                    /// (Always examine .tag() first to make sure this is the correct variant!)
                    /// Panics in debug builds if the .tag() doesn't return Blah.
                    pub unsafe fn as_Blah(&self) -> i32 {
                        debug_assert_eq!(self.tag(), tag_NonRecursive::Blah);
                        self.Blah
                    }

                    /// Construct a tag named Foo, with the appropriate payload
                    pub fn Foo(payload: roc_std::RocStr) -> Self {
                        let mut answer = Self {
                            Foo: core::mem::ManuallyDrop::new(payload)
                        };

                        answer.set_discriminant(tag_NonRecursive::Foo);

                        answer
                    }

                    /// Unsafely assume the given NonRecursive has a .tag() of Foo and convert it to Foo's payload.
                    /// (Always examine .tag() first to make sure this is the correct variant!)
                    /// Panics in debug builds if the .tag() doesn't return Foo.
                    pub unsafe fn into_Foo(mut self) -> roc_std::RocStr {
                        debug_assert_eq!(self.tag(), tag_NonRecursive::Foo);
                        core::mem::ManuallyDrop::take(&mut self.Foo)
                    }

                    /// Unsafely assume the given NonRecursive has a .tag() of Foo and return its payload.
                    /// (Always examine .tag() first to make sure this is the correct variant!)
                    /// Panics in debug builds if the .tag() doesn't return Foo.
                    pub unsafe fn as_Foo(&self) -> &roc_std::RocStr {
                        debug_assert_eq!(self.tag(), tag_NonRecursive::Foo);
                        &self.Foo
                    }
                }

                impl Drop for NonRecursive {
                    fn drop(&mut self) {
                        match self.tag() {
                            tag_NonRecursive::Bar => {}
                            tag_NonRecursive::Baz => {}
                            tag_NonRecursive::Blah => {}
                            tag_NonRecursive::Foo => unsafe { core::mem::ManuallyDrop::drop(&mut self.Foo) },
                        }
                    }
                }

                impl PartialEq for NonRecursive {
                    fn eq(&self, other: &Self) -> bool {
                        if self.tag() != other.tag() {
                            return false;
                        }

                        unsafe {
                            match self.tag() {
                                tag_NonRecursive::Bar => self.Bar == other.Bar,
                                tag_NonRecursive::Baz => true,
                                tag_NonRecursive::Blah => self.Blah == other.Blah,
                                tag_NonRecursive::Foo => self.Foo == other.Foo,
                            }
                        }
                    }
                }

                impl Eq for NonRecursive {}

                impl PartialOrd for NonRecursive {
                    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
                        match self.tag().partial_cmp(&other.tag()) {
                            Some(core::cmp::Ordering::Equal) => {}
                            not_eq => return not_eq,
                        }

                        unsafe {
                            match self.tag() {
                                tag_NonRecursive::Bar => self.Bar.partial_cmp(&other.Bar),
                                tag_NonRecursive::Baz => Some(core::cmp::Ordering::Equal),
                                tag_NonRecursive::Blah => self.Blah.partial_cmp(&other.Blah),
                                tag_NonRecursive::Foo => self.Foo.partial_cmp(&other.Foo),
                            }
                        }
                    }
                }

                impl Ord for NonRecursive {
                    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
                        match self.tag().cmp(&other.tag()) {
                            core::cmp::Ordering::Equal => {}
                            not_eq => return not_eq,
                        }

                        unsafe {
                            match self.tag() {
                                tag_NonRecursive::Bar => self.Bar.cmp(&other.Bar),
                                tag_NonRecursive::Baz => core::cmp::Ordering::Equal,
                                tag_NonRecursive::Blah => self.Blah.cmp(&other.Blah),
                                tag_NonRecursive::Foo => self.Foo.cmp(&other.Foo),
                            }
                        }
                    }
                }

                impl Clone for NonRecursive {
                    fn clone(&self) -> Self {
                        let mut answer = unsafe {
                            match self.tag() {
                                tag_NonRecursive::Bar => Self {
                                        Bar: self.Bar.clone(),
                                    },
                                tag_NonRecursive::Baz => core::mem::transmute::<
                                        core::mem::MaybeUninit<NonRecursive>,
                                        NonRecursive,
                                    >(core::mem::MaybeUninit::uninit()),
                                tag_NonRecursive::Blah => Self {
                                        Blah: self.Blah.clone(),
                                    },
                                tag_NonRecursive::Foo => Self {
                                        Foo: self.Foo.clone(),
                                    },
                            }

                        };

                        answer.set_discriminant(self.tag());

                        answer
                    }
                }

                impl core::hash::Hash for NonRecursive {
                    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
                        match self.tag() {
                            tag_NonRecursive::Bar => unsafe {
                                tag_NonRecursive::Bar.hash(state);
                                self.Bar.hash(state);
                            },
                            tag_NonRecursive::Baz => tag_NonRecursive::Baz.hash(state),
                            tag_NonRecursive::Blah => unsafe {
                                tag_NonRecursive::Blah.hash(state);
                                self.Blah.hash(state);
                            },
                            tag_NonRecursive::Foo => unsafe {
                                tag_NonRecursive::Foo.hash(state);
                                self.Foo.hash(state);
                            },
                        }
                    }
                }

                impl core::fmt::Debug for NonRecursive {
                    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                        f.write_str("NonRecursive::")?;

                        unsafe {
                            match self.tag() {
                                tag_NonRecursive::Bar => f.debug_tuple("Bar").field(&self.Bar).finish(),
                                tag_NonRecursive::Baz => f.write_str("Baz"),
                                tag_NonRecursive::Blah => f.debug_tuple("Blah").field(&self.Blah).finish(),
                                tag_NonRecursive::Foo => f.debug_tuple("Foo").field(&*self.Foo).finish(),
                            }
                        }
                    }
                }

            "#
            )
        );
    }

    #[test]
    fn tag_union_enumeration() {
        let module = indoc!(
            r#"
            Enumeration : [ Blah, Foo, Bar, ]

            main : Enumeration
            main = Foo
        "#
        );

        assert_eq!(
            generate_bindings(module)
                .strip_prefix('\n')
                .unwrap_or_default(),
            indoc!(
                r#"
                #[derive(Clone, Copy, Eq, Ord, Hash, PartialEq, PartialOrd)]
                #[repr(u8)]
                pub enum Enumeration {
                    Bar = 0,
                    Blah = 1,
                    Foo = 2,
                }

                impl core::fmt::Debug for Enumeration {
                    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                        match self {
                            Self::Bar => f.write_str("Enumeration::Bar"),
                            Self::Blah => f.write_str("Enumeration::Blah"),
                            Self::Foo => f.write_str("Enumeration::Foo"),
                        }
                    }
                }
            "#
            )
        );
    }

    #[test]
    fn single_tag_union_with_payloads() {
        let module = indoc!(
            r#"
            UserId : [ Id U32 Str ]

            main : UserId
            main = Id 42 "blah"
        "#
        );

        assert_eq!(
            generate_bindings(module)
                .strip_prefix('\n')
                .unwrap_or_default(),
            indoc!(
                r#"
                #[derive(Clone, Debug, Default, Eq, Ord, Hash, PartialEq, PartialOrd)]
                #[repr(C)]
                pub struct UserId {
                    pub f1: roc_std::RocStr,
                    pub f0: u32,
                }
            "#
            )
        );
    }

    #[test]
    fn single_tag_union_with_one_payload_field() {
        let module = indoc!(
            r#"
            UserId : [ Id Str ]

            main : UserId
            main = Id "blah"
        "#
        );

        assert_eq!(
            generate_bindings(module)
                .strip_prefix('\n')
                .unwrap_or_default(),
            indoc!(
                r#"
                #[derive(Clone, Debug, Default, Eq, Ord, Hash, PartialEq, PartialOrd)]
                #[repr(transparent)]
                pub struct UserId(roc_std::RocStr);
            "#
            )
        );
    }

    #[test]
    fn cons_list_of_strings() {
        let module = indoc!(
            r#"
            StrConsList : [ Nil, Cons Str StrConsList ]

            main : StrConsList
            main = Cons "Hello, " (Cons "World!" Nil)
        "#
        );

        assert_eq!(
            generate_bindings(module)
                .strip_prefix('\n')
                .unwrap_or_default(),
            indoc!(
                r#"
                #[derive(Clone, Copy, Eq, Ord, Hash, PartialEq, PartialOrd)]
                #[repr(u8)]
                pub enum tag_StrConsList {
                    Cons = 0,
                    Nil = 1,
                }

                impl core::fmt::Debug for tag_StrConsList {
                    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                        match self {
                            Self::Cons => f.write_str("tag_StrConsList::Cons"),
                            Self::Nil => f.write_str("tag_StrConsList::Nil"),
                        }
                    }
                }

                #[derive(Clone, Eq, Ord, Hash, PartialEq, PartialOrd)]
                #[repr(C)]
                pub struct StrConsList {
                    pointer: *mut core::mem::ManuallyDrop<roc_std::RocStr>,
                }

                impl StrConsList {
                    pub fn tag(&self) -> tag_StrConsList {
                        if self.pointer.is_null() {
                            tag_StrConsList::Nil
                        } else {
                            tag_StrConsList::Cons
                        }
                    }

                    /// Construct a tag named Cons, with the appropriate payload
                    pub fn Cons(payload: roc_std::RocStr) -> Self {
                        let size = core::mem::size_of::<roc_std::RocStr>();
                        let align = core::mem::align_of::<roc_std::RocStr>();

                        unsafe {
                            let pointer = crate::roc_alloc(size, align as u32) as *mut core::mem::ManuallyDrop<roc_std::RocStr>;

                            *pointer = core::mem::ManuallyDrop::new(payload);

                            Self { pointer }
                        }
                    }

                    /// Unsafely assume the given StrConsList has a .tag() of Cons and convert it to Cons's payload.
                    /// (Always examine .tag() first to make sure this is the correct variant!)
                    /// Panics in debug builds if the .tag() doesn't return Cons.
                    pub unsafe fn into_Cons(self) -> roc_std::RocStr {
                        debug_assert_eq!(self.tag(), tag_StrConsList::Cons);

                        let payload = core::mem::ManuallyDrop::take(&mut *self.pointer);
                        let align = core::mem::align_of::<roc_std::RocStr>() as u32;

                        crate::roc_dealloc(self.pointer as *mut core::ffi::c_void, align);

                        payload
                    }

                    /// Unsafely assume the given StrConsList has a .tag() of Cons and return its payload.
                    /// (Always examine .tag() first to make sure this is the correct variant!)
                    /// Panics in debug builds if the .tag() doesn't return Cons.
                    pub unsafe fn as_Cons(&self) -> &roc_std::RocStr {
                        debug_assert_eq!(self.tag(), tag_StrConsList::Cons);
                        &*self.pointer
                    }

                    /// Construct a tag named Nil
                    pub fn Nil() -> Self {
                        Self {
                            pointer: core::ptr::null_mut(),
                        }
                    }

                    /// Other `into_` methods return a payload, but since the Nil tag
                    /// has no payload, this does nothing and is only here for completeness.
                    pub fn into_Nil(self) {
                        ()
                    }

                    /// Other `as` methods return a payload, but since the Nil tag
                    /// has no payload, this does nothing and is only here for completeness.
                    pub unsafe fn as_Nil(&self) {
                        ()
                    }
                }

                impl Drop for StrConsList {
                    fn drop(&mut self) {
                        if !self.pointer.is_null() {
                            let payload = unsafe { &*self.pointer };
                            let align = core::mem::align_of::<roc_std::RocStr>() as u32;

                            unsafe {
                                crate::roc_dealloc(self.pointer as *mut core::ffi::c_void, align);
                            }

                            drop(payload);
                        }
                    }
                }

                impl core::fmt::Debug for StrConsList {
                    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                        if self.pointer.is_null() {
                            f.write_str("StrConsList::Nil")
                        } else {
                            f.write_str("StrConsList::")?;

                            unsafe { f.debug_tuple("Cons").field(&**self.pointer).finish() }
                        }
                    }
                }

            "#
            )
        );
    }

    #[test]
    fn cons_list_of_ints() {
        let module = indoc!(
            r#"
            IntConsList : [ Empty, Prepend U16 IntConsList ]

            main : IntConsList
            main = Prepend 42 (Prepend 26 Empty)
        "#
        );

        assert_eq!(
            generate_bindings(module)
                .strip_prefix('\n')
                .unwrap_or_default(),
            indoc!(
                r#"
                #[derive(Clone, Copy, Eq, Ord, Hash, PartialEq, PartialOrd)]
                #[repr(u8)]
                pub enum tag_IntConsList {
                    Empty = 0,
                    Prepend = 1,
                }

                impl core::fmt::Debug for tag_IntConsList {
                    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                        match self {
                            Self::Empty => f.write_str("tag_IntConsList::Empty"),
                            Self::Prepend => f.write_str("tag_IntConsList::Prepend"),
                        }
                    }
                }

                #[derive(Clone, Eq, Ord, Hash, PartialEq, PartialOrd)]
                #[repr(C)]
                pub struct IntConsList {
                    pointer: *mut u16,
                }

                impl IntConsList {
                    pub fn tag(&self) -> tag_IntConsList {
                        if self.pointer.is_null() {
                            tag_IntConsList::Empty
                        } else {
                            tag_IntConsList::Prepend
                        }
                    }

                    /// Construct a tag named Prepend, with the appropriate payload
                    pub fn Prepend(payload: u16) -> Self {
                        let size = core::mem::size_of::<u16>();
                        let align = core::mem::align_of::<u16>();

                        unsafe {
                            let pointer = crate::roc_alloc(size, align as u32) as *mut u16;

                            *pointer = payload;

                            Self { pointer }
                        }
                    }

                    /// Unsafely assume the given IntConsList has a .tag() of Prepend and convert it to Prepend's payload.
                    /// (Always examine .tag() first to make sure this is the correct variant!)
                    /// Panics in debug builds if the .tag() doesn't return Prepend.
                    pub unsafe fn into_Prepend(self) -> u16 {
                        debug_assert_eq!(self.tag(), tag_IntConsList::Prepend);

                        let payload = *self.pointer;
                        let align = core::mem::align_of::<u16>() as u32;

                        crate::roc_dealloc(self.pointer as *mut core::ffi::c_void, align);

                        payload
                    }

                    /// Unsafely assume the given IntConsList has a .tag() of Prepend and return its payload.
                    /// (Always examine .tag() first to make sure this is the correct variant!)
                    /// Panics in debug builds if the .tag() doesn't return Prepend.
                    pub unsafe fn as_Prepend(&self) -> u16 {
                        debug_assert_eq!(self.tag(), tag_IntConsList::Prepend);
                        *self.pointer
                    }

                    /// Construct a tag named Empty
                    pub fn Empty() -> Self {
                        Self {
                            pointer: core::ptr::null_mut(),
                        }
                    }

                    /// Other `into_` methods return a payload, but since the Empty tag
                    /// has no payload, this does nothing and is only here for completeness.
                    pub fn into_Empty(self) {
                        ()
                    }

                    /// Other `as` methods return a payload, but since the Empty tag
                    /// has no payload, this does nothing and is only here for completeness.
                    pub unsafe fn as_Empty(&self) {
                        ()
                    }
                }

                impl Drop for IntConsList {
                    fn drop(&mut self) {
                        if !self.pointer.is_null() {
                            let payload = unsafe { &*self.pointer };
                            let align = core::mem::align_of::<u16>() as u32;

                            unsafe {
                                crate::roc_dealloc(self.pointer as *mut core::ffi::c_void, align);
                            }

                            drop(payload);
                        }
                    }
                }

                impl core::fmt::Debug for IntConsList {
                    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                        if self.pointer.is_null() {
                            f.write_str("IntConsList::Empty")
                        } else {
                            f.write_str("IntConsList::")?;

                            unsafe { f.debug_tuple("Prepend").field(&*self.pointer).finish() }
                        }
                    }
                }

            "#
            )
        );
    }
}
