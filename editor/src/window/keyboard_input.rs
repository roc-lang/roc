#[derive(Debug)]
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

impl Modifiers {
    pub fn cmd_or_ctrl(&self) -> bool {
        #[cfg(target_os = "macos")]
        let active = self.logo;

        #[cfg(not(target_os = "macos"))]
        let active = self.ctrl;

        active
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

#[cfg(test)]
pub mod test_modifiers {
    use crate::window::keyboard_input::Modifiers;

    pub fn ctrl_cmd_shift() -> Modifiers {
        #[cfg(target_os = "macos")]
        let mods = Modifiers {
            shift: true,
            ctrl: false,
            alt: false,
            logo: true,
        };

        #[cfg(not(target_os = "macos"))]
        let mods = Modifiers {
            shift: true,
            ctrl: true,
            alt: false,
            logo: false,
        };

        mods
    }
}
