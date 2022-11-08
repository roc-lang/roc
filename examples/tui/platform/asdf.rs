// ⚠️ GENERATED CODE ⚠️ - this entire file was generated by the `roc glue` CLI command

#![allow(unused_unsafe)]
#![allow(dead_code)]
#![allow(unused_mut)]
#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(non_upper_case_globals)]
#![allow(clippy::undocumented_unsafe_blocks)]
#![allow(clippy::redundant_static_lifetimes)]
#![allow(clippy::unused_unit)]
#![allow(clippy::missing_safety_doc)]
#![allow(clippy::let_and_return)]
#![allow(clippy::missing_safety_doc)]
#![allow(clippy::redundant_static_lifetimes)]
#![allow(clippy::needless_borrow)]
#![allow(clippy::clone_on_copy)]

#[cfg(any(
    target_arch = "arm",
    target_arch = "aarch64",
    target_arch = "wasm32",
    target_arch = "x86",
    target_arch = "x86_64"
))]
#[repr(transparent)]
#[derive(Clone, Eq, Ord, Hash, PartialEq, PartialOrd)]
pub struct Elem {
    f0: roc_std::RocList<roc_std::RocList<Span>>,
    f1: ParagraphConfig,
}


#[cfg(any(
    target_arch = "arm",
    target_arch = "aarch64",
    target_arch = "wasm32",
    target_arch = "x86",
    target_arch = "x86_64"
))]
#[derive(Clone, Debug, Eq, Ord, Hash, PartialEq, PartialOrd)]
#[repr(C)]
pub struct Span {
    pub style: Styles,
    pub text: roc_std::RocStr,
}

#[cfg(any(
    target_arch = "arm",
    target_arch = "aarch64",
    target_arch = "wasm32",
    target_arch = "x86",
    target_arch = "x86_64"
))]
#[derive(Clone, Copy, Eq, Ord, Hash, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum discriminant_Event {
    KeyDown = 0,
    KeyUp = 1,
    Resize = 2,
    Tick = 3,
}

impl core::fmt::Debug for discriminant_Event {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::KeyDown => f.write_str("discriminant_Event::KeyDown"),
            Self::KeyUp => f.write_str("discriminant_Event::KeyUp"),
            Self::Resize => f.write_str("discriminant_Event::Resize"),
            Self::Tick => f.write_str("discriminant_Event::Tick"),
        }
    }
}

#[cfg(any(
    target_arch = "arm",
    target_arch = "aarch64",
    target_arch = "wasm32",
    target_arch = "x86",
    target_arch = "x86_64"
))]
#[repr(C)]
pub union Event {
    KeyDown: KeyCode,
    KeyUp: KeyCode,
    Resize: Bounds,
    Tick: roc_std::U128,
    _sizer: [u8; 32],
}

#[cfg(any(
    target_arch = "arm",
    target_arch = "aarch64",
    target_arch = "wasm32",
    target_arch = "x86",
    target_arch = "x86_64"
))]
#[derive(Clone, Debug, Default, Eq, Ord, Hash, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Model {
    pub text: roc_std::RocStr,
}

#[cfg(any(
    target_arch = "arm",
    target_arch = "aarch64",
    target_arch = "wasm32",
    target_arch = "x86",
    target_arch = "x86_64"
))]
#[derive(Clone, Copy, Eq, Ord, Hash, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum BorderModifier {
    ALL = 0,
    BOTTOM = 1,
    LEFT = 2,
    NONE = 3,
    RIGHT = 4,
    TOP = 5,
}

impl core::fmt::Debug for BorderModifier {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::ALL => f.write_str("BorderModifier::ALL"),
            Self::BOTTOM => f.write_str("BorderModifier::BOTTOM"),
            Self::LEFT => f.write_str("BorderModifier::LEFT"),
            Self::NONE => f.write_str("BorderModifier::NONE"),
            Self::RIGHT => f.write_str("BorderModifier::RIGHT"),
            Self::TOP => f.write_str("BorderModifier::TOP"),
        }
    }
}

#[cfg(any(
    target_arch = "arm",
    target_arch = "aarch64",
    target_arch = "wasm32",
    target_arch = "x86",
    target_arch = "x86_64"
))]
#[derive(Clone, Copy, Eq, Ord, Hash, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum TextModifier {
    BOLD = 0,
    CROSSEDOUT = 1,
    DIM = 2,
    HIDDEN = 3,
    ITALIC = 4,
    RAPIDBLINK = 5,
    REVERSED = 6,
    SLOWBLINK = 7,
    UNDERLINED = 8,
}

impl core::fmt::Debug for TextModifier {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::BOLD => f.write_str("TextModifier::BOLD"),
            Self::CROSSEDOUT => f.write_str("TextModifier::CROSSEDOUT"),
            Self::DIM => f.write_str("TextModifier::DIM"),
            Self::HIDDEN => f.write_str("TextModifier::HIDDEN"),
            Self::ITALIC => f.write_str("TextModifier::ITALIC"),
            Self::RAPIDBLINK => f.write_str("TextModifier::RAPIDBLINK"),
            Self::REVERSED => f.write_str("TextModifier::REVERSED"),
            Self::SLOWBLINK => f.write_str("TextModifier::SLOWBLINK"),
            Self::UNDERLINED => f.write_str("TextModifier::UNDERLINED"),
        }
    }
}

#[cfg(any(
    target_arch = "arm",
    target_arch = "aarch64",
    target_arch = "wasm32",
    target_arch = "x86",
    target_arch = "x86_64"
))]
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd)]
#[repr(C)]
pub struct Bounds {
    pub height: f32,
    pub width: f32,
}

#[cfg(any(
    target_arch = "arm",
    target_arch = "aarch64",
    target_arch = "wasm32",
    target_arch = "x86",
    target_arch = "x86_64"
))]
#[derive(Clone, Copy, Debug, Eq, Ord, Hash, PartialEq, PartialOrd)]
#[repr(C)]
pub struct R1 {
    pub init: TODO_roc_function_65,
    pub render: TODO_roc_function_69,
    pub update: TODO_roc_function_67,
}

#[cfg(any(
    target_arch = "arm",
    target_arch = "aarch64",
    target_arch = "wasm32",
    target_arch = "x86",
    target_arch = "x86_64"
))]
#[derive(Clone, Copy, Eq, Ord, Hash, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum KeyCode {
    Down = 0,
    Left = 1,
    Other = 2,
    Right = 3,
    Up = 4,
}

impl core::fmt::Debug for KeyCode {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Down => f.write_str("KeyCode::Down"),
            Self::Left => f.write_str("KeyCode::Left"),
            Self::Other => f.write_str("KeyCode::Other"),
            Self::Right => f.write_str("KeyCode::Right"),
            Self::Up => f.write_str("KeyCode::Up"),
        }
    }
}

#[cfg(any(
    target_arch = "arm",
    target_arch = "aarch64",
    target_arch = "wasm32",
    target_arch = "x86",
    target_arch = "x86_64"
))]
#[derive(Clone, Debug, Eq, Ord, Hash, PartialEq, PartialOrd)]
#[repr(C)]
pub struct ParagraphConfig {
    pub borderStyle: Styles,
    pub borders: roc_std::RocList<BorderModifier>,
    pub style: Styles,
    pub title: roc_std::RocStr,
    pub titleStyle: Styles,
    pub borderType: BorderType,
    pub titleAlignment: Alignment,
}

#[cfg(any(
    target_arch = "arm",
    target_arch = "aarch64",
    target_arch = "wasm32",
    target_arch = "x86",
    target_arch = "x86_64"
))]
#[derive(Clone, Copy, Eq, Ord, Hash, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum Alignment {
    Center = 0,
    Left = 1,
    Right = 2,
}

impl core::fmt::Debug for Alignment {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Center => f.write_str("Alignment::Center"),
            Self::Left => f.write_str("Alignment::Left"),
            Self::Right => f.write_str("Alignment::Right"),
        }
    }
}

#[cfg(any(
    target_arch = "arm",
    target_arch = "aarch64",
    target_arch = "wasm32",
    target_arch = "x86",
    target_arch = "x86_64"
))]
#[derive(Clone, Copy, Eq, Ord, Hash, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum BorderType {
    Double = 0,
    Plain = 1,
    Rounded = 2,
    Thick = 3,
}

impl core::fmt::Debug for BorderType {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Double => f.write_str("BorderType::Double"),
            Self::Plain => f.write_str("BorderType::Plain"),
            Self::Rounded => f.write_str("BorderType::Rounded"),
            Self::Thick => f.write_str("BorderType::Thick"),
        }
    }
}

#[cfg(any(
    target_arch = "arm",
    target_arch = "aarch64",
    target_arch = "wasm32",
    target_arch = "x86",
    target_arch = "x86_64"
))]
#[derive(Clone, Debug, Eq, Ord, Hash, PartialEq, PartialOrd)]
#[repr(C)]
pub struct Styles {
    pub modifiers: roc_std::RocList<TextModifier>,
    pub bg: Color,
    pub fg: Color,
}

#[cfg(any(
    target_arch = "arm",
    target_arch = "aarch64",
    target_arch = "wasm32",
    target_arch = "x86",
    target_arch = "x86_64"
))]
#[derive(Clone, Copy, Eq, Ord, Hash, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum Color {
    Black = 0,
    Blue = 1,
    Cyan = 2,
    DarkGray = 3,
    Gray = 4,
    Green = 5,
    LightBlue = 6,
    LightCyan = 7,
    LightGreen = 8,
    LightMagenta = 9,
    LightRed = 10,
    LightYellow = 11,
    Magenta = 12,
    Red = 13,
    White = 14,
    Yellow = 15,
}

impl core::fmt::Debug for Color {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Black => f.write_str("Color::Black"),
            Self::Blue => f.write_str("Color::Blue"),
            Self::Cyan => f.write_str("Color::Cyan"),
            Self::DarkGray => f.write_str("Color::DarkGray"),
            Self::Gray => f.write_str("Color::Gray"),
            Self::Green => f.write_str("Color::Green"),
            Self::LightBlue => f.write_str("Color::LightBlue"),
            Self::LightCyan => f.write_str("Color::LightCyan"),
            Self::LightGreen => f.write_str("Color::LightGreen"),
            Self::LightMagenta => f.write_str("Color::LightMagenta"),
            Self::LightRed => f.write_str("Color::LightRed"),
            Self::LightYellow => f.write_str("Color::LightYellow"),
            Self::Magenta => f.write_str("Color::Magenta"),
            Self::Red => f.write_str("Color::Red"),
            Self::White => f.write_str("Color::White"),
            Self::Yellow => f.write_str("Color::Yellow"),
        }
    }
}

impl Elem {
    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    /// A tag named Paragraph, with the given payload.
    pub fn Paragraph(f0: roc_std::RocList<roc_std::RocList<Span>>, f1: ParagraphConfig) -> Self {
        Self {
            f0,
            f1,
        }
    }

    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    /// Since `Paragraph` only has one tag (namely, `Paragraph`),
    /// convert it to `Paragraph`'s payload.
    pub fn into_Paragraph(self) -> (roc_std::RocList<roc_std::RocList<Span>>, ParagraphConfig) {
        (self.f0, self.f1)
    }

    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    /// Since `Paragraph` only has one tag (namely, `Paragraph`),
    /// convert it to `Paragraph`'s payload.
    pub fn as_Paragraph(&self) -> (&roc_std::RocList<roc_std::RocList<Span>>, &ParagraphConfig) {
        (&self.f0, &self.f1)
    }
}

impl core::fmt::Debug for Elem {
    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            f.debug_tuple("Elem::Paragraph")                .field(&self.f0)                .field(&self.f1)                .finish()    }

}

impl Event {
    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    /// Returns which variant this tag union holds. Note that this never includes a payload!
    pub fn discriminant(&self) -> discriminant_Event {
        unsafe {
            let bytes = core::mem::transmute::<&Self, &[u8; core::mem::size_of::<Self>()]>(self);

            core::mem::transmute::<u8, discriminant_Event>(*bytes.as_ptr().add(16))
        }
    }

    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    /// Internal helper
    fn set_discriminant(&mut self, discriminant: discriminant_Event) {
        let discriminant_ptr: *mut discriminant_Event = (self as *mut Event).cast();

        unsafe {
            *(discriminant_ptr.add(16)) = discriminant;
        }
    }

    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    /// Construct a tag named `KeyDown`, with the appropriate payload
    pub fn KeyDown(arg: KeyCode) -> Self {
            let mut answer = Self {
                KeyDown: arg
            };

            answer.set_discriminant(discriminant_Event::KeyDown);

            answer
    }

    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    /// Unsafely assume the given `Event` has a `.discriminant()` of `KeyDown` and convert it to `KeyDown`'s payload.
            /// (Always examine `.discriminant()` first to make sure this is the correct variant!)
            /// Panics in debug builds if the `.discriminant()` doesn't return `KeyDown`.
            pub unsafe fn into_KeyDown(self) -> KeyCode {
                debug_assert_eq!(self.discriminant(), discriminant_Event::KeyDown);
        let payload = self.KeyDown;

        payload
    }

    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    /// Unsafely assume the given `Event` has a `.discriminant()` of `KeyDown` and return its payload.
            /// (Always examine `.discriminant()` first to make sure this is the correct variant!)
            /// Panics in debug builds if the `.discriminant()` doesn't return `KeyDown`.
            pub unsafe fn as_KeyDown(&self) -> &KeyCode {
                debug_assert_eq!(self.discriminant(), discriminant_Event::KeyDown);
        let payload = &self.KeyDown;

        &payload
    }

    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    /// Construct a tag named `KeyUp`, with the appropriate payload
    pub fn KeyUp(arg: KeyCode) -> Self {
            let mut answer = Self {
                KeyUp: arg
            };

            answer.set_discriminant(discriminant_Event::KeyUp);

            answer
    }

    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    /// Unsafely assume the given `Event` has a `.discriminant()` of `KeyUp` and convert it to `KeyUp`'s payload.
            /// (Always examine `.discriminant()` first to make sure this is the correct variant!)
            /// Panics in debug builds if the `.discriminant()` doesn't return `KeyUp`.
            pub unsafe fn into_KeyUp(self) -> KeyCode {
                debug_assert_eq!(self.discriminant(), discriminant_Event::KeyUp);
        let payload = self.KeyUp;

        payload
    }

    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    /// Unsafely assume the given `Event` has a `.discriminant()` of `KeyUp` and return its payload.
            /// (Always examine `.discriminant()` first to make sure this is the correct variant!)
            /// Panics in debug builds if the `.discriminant()` doesn't return `KeyUp`.
            pub unsafe fn as_KeyUp(&self) -> &KeyCode {
                debug_assert_eq!(self.discriminant(), discriminant_Event::KeyUp);
        let payload = &self.KeyUp;

        &payload
    }

    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    /// Construct a tag named `Resize`, with the appropriate payload
    pub fn Resize(arg0: Bounds) -> Self {
            let mut answer = Self {
                Resize: core::mem::ManuallyDrop::new(arg0)
            };

            answer.set_discriminant(discriminant_Event::Resize);

            answer
    }

    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    /// Unsafely assume the given `Event` has a `.discriminant()` of `Resize` and convert it to `Resize`'s payload.
            /// (Always examine `.discriminant()` first to make sure this is the correct variant!)
            /// Panics in debug builds if the `.discriminant()` doesn't return `Resize`.
            pub unsafe fn into_Resize(self) -> Bounds {
                debug_assert_eq!(self.discriminant(), discriminant_Event::Resize);
        let payload = self.Resize;

        
        payload
    }

    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    /// Unsafely assume the given `Event` has a `.discriminant()` of `Resize` and return its payload.
            /// (Always examine `.discriminant()` first to make sure this is the correct variant!)
            /// Panics in debug builds if the `.discriminant()` doesn't return `Resize`.
            pub unsafe fn as_Resize(&self) -> &Bounds {
                debug_assert_eq!(self.discriminant(), discriminant_Event::Resize);
        let payload = &self.Resize;

        
        payload
    }

    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    /// Construct a tag named `Tick`, with the appropriate payload
    pub fn Tick(arg: roc_std::U128) -> Self {
            let mut answer = Self {
                Tick: arg
            };

            answer.set_discriminant(discriminant_Event::Tick);

            answer
    }

    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    /// Unsafely assume the given `Event` has a `.discriminant()` of `Tick` and convert it to `Tick`'s payload.
            /// (Always examine `.discriminant()` first to make sure this is the correct variant!)
            /// Panics in debug builds if the `.discriminant()` doesn't return `Tick`.
            pub unsafe fn into_Tick(self) -> roc_std::U128 {
                debug_assert_eq!(self.discriminant(), discriminant_Event::Tick);
        let payload = self.Tick;

        payload
    }

    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    /// Unsafely assume the given `Event` has a `.discriminant()` of `Tick` and return its payload.
            /// (Always examine `.discriminant()` first to make sure this is the correct variant!)
            /// Panics in debug builds if the `.discriminant()` doesn't return `Tick`.
            pub unsafe fn as_Tick(&self) -> &roc_std::U128 {
                debug_assert_eq!(self.discriminant(), discriminant_Event::Tick);
        let payload = &self.Tick;

        &payload
    }
}

impl PartialEq for Event {
    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    fn eq(&self, other: &Self) -> bool {
            if self.discriminant() != other.discriminant() {
                return false;
            }

            unsafe {
            match self.discriminant() {
                discriminant_Event::KeyDown => self.KeyDown == other.KeyDown,
                discriminant_Event::KeyUp => self.KeyUp == other.KeyUp,
                discriminant_Event::Resize => self.Resize == other.Resize,
                discriminant_Event::Tick => self.Tick == other.Tick,
            }
        }
    }
}

impl PartialOrd for Event {
    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        match self.discriminant().partial_cmp(&other.discriminant()) {
            Some(core::cmp::Ordering::Equal) => {}
            not_eq => return not_eq,
        }

        unsafe {
            match self.discriminant() {
                discriminant_Event::KeyDown => self.KeyDown.partial_cmp(&other.KeyDown),
                discriminant_Event::KeyUp => self.KeyUp.partial_cmp(&other.KeyUp),
                discriminant_Event::Resize => self.Resize.partial_cmp(&other.Resize),
                discriminant_Event::Tick => self.Tick.partial_cmp(&other.Tick),
            }
        }
    }
}

impl Copy for Event {}

impl Clone for Event {
    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    fn clone(&self) -> Self {
        let mut answer = unsafe {
            match self.discriminant() {
                discriminant_Event::KeyDown => Self {
                    KeyDown: self.KeyDown.clone(),
                },
                discriminant_Event::KeyUp => Self {
                    KeyUp: self.KeyUp.clone(),
                },
                discriminant_Event::Resize => Self {
                    Resize: self.Resize.clone(),
                },
                discriminant_Event::Tick => Self {
                    Tick: self.Tick.clone(),
                },
            }

        };

        answer.set_discriminant(self.discriminant());

        answer
    }
}

impl core::fmt::Debug for Event {
    #[cfg(any(
        target_arch = "arm",
        target_arch = "aarch64",
        target_arch = "wasm32",
        target_arch = "x86",
        target_arch = "x86_64"
    ))]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str("Event::")?;

        unsafe {
            match self.discriminant() {
                discriminant_Event::KeyDown => f.debug_tuple("KeyDown")
        .field(&self.KeyDown)
        .finish(),
                discriminant_Event::KeyUp => f.debug_tuple("KeyUp")
        .field(&self.KeyUp)
        .finish(),
                discriminant_Event::Resize => f.debug_tuple("Resize")
        .field(&self.Resize)
        .finish(),
                discriminant_Event::Tick => f.debug_tuple("Tick")
        .field(&self.Tick)
        .finish(),
            }
        }
    }
}
