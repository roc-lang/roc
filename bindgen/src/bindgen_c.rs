use std::io;

static TEMPLATE: &[u8] = include_bytes!("../templates/template.c");

pub fn write_template(writer: &mut impl io::Write) -> io::Result<()> {
    writer.write(TEMPLATE)?;

    Ok(())
}

// pub fn write_bindings(_writer: &mut impl io::Write) -> io::Result<()> {
// extern struct RocStr roc__mainForHost_1_exposed();

// int main() {
//   struct RocStr str = roc__mainForHost_1_exposed();

//   // Determine str_len and the str_bytes pointer,
//   // taking into account the small string optimization.
//   size_t str_len = roc_str_len(str);
//   char* str_bytes;

//   if (is_small_str(str)) {
//     str_bytes = (char*)&str;
//   } else {
//     str_bytes = str.bytes;
//   }

//   // Write to stdout
//   if (write(1, str_bytes, str_len) >= 0) {
//     // Writing succeeded!
//     return 0;
//   } else {
//     printf("Error writing to stdout: %s\n", strerror(errno));

//     return 1;
//   }
// }

//     Ok(())
// }
