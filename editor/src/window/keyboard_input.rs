pub struct Modifiers {
    pub shift: bool,
    pub ctrl: bool,
    pub alt: bool,
    pub logo: bool,
}

impl Default for Modifiers {
    fn default() -> Self {
        Self {
            shift: false,
            ctrl: false,
            alt: false,
            logo: false,
        }
    }
}

pub fn no_mods() -> Modifiers {
    Modifiers {
        shift: false,
        ctrl: false,
        alt: false,
        logo: false,
    }
}

pub fn from_winit(winit_mods: &winit::event::ModifiersState) -> Modifiers {
    Modifiers {
        shift: winit_mods.shift(),
        ctrl: winit_mods.ctrl(),
        alt: winit_mods.alt(),
        logo: winit_mods.logo(),
    }
}
