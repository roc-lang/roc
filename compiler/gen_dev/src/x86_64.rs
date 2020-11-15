use crate::Backend;

pub struct X86_64Backend {
    // This is gonna need to include a lot of data. Right now I can think of quite a few.
// Registers order by priority with info of what data is stored in them.
// Scope with knows were all variables are currently stored.X86_64Backend

// Since this is x86_64 the calling convetions is really just windows or linux/macos.
// Hopefully this will be easy to extract into a trait somehow. Cause probably don't want if's everywhere.
// Also, don't really want to build an x86_64-win backend specifically for it.

// function parameter registers listed by order. Need to know the float equivalent registers as well.
// Probably need to encode stack parameter knowledge here too.
// return parameter register. This includes dealing with multiple value returns.
}

impl Backend for X86_64Backend {
    fn new() -> Self {
        X86_64Backend {}
    }

    fn reset(&mut self) {}
}
