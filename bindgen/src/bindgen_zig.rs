use std::io;

static TEMPLATE: &[u8] = include_bytes!("../templates/template.zig");

pub fn write_template(writer: &mut impl io::Write) -> io::Result<()> {
    writer.write(TEMPLATE)?;

    Ok(())
}

pub fn write_bindings(_writer: &mut impl io::Write) -> io::Result<()> {
    // extern "C" {
    //     #[link_name = "roc__mainForHost_1_exposed"]
    //     fn roc_main() -> RocStr;
    // }

    Ok(())
}
