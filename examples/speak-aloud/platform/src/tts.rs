use std::{io::Write, process};

pub fn speak(text: &str) -> Result<(), ()> {
    let mut festival_child = process::Command::new("festival")
        .arg("--pipe")
        .stdin(process::Stdio::piped())
        .spawn()
        .expect("failed to spawn festival");

    festival_child
        .stdin
        .as_mut()
        .expect("failed to acquire festival's stdin")
        .write_all(format!(r#"(SayText "{}")"#, text).as_bytes())
        .expect("failed to write to festival's stdin");

    println!(r#"Speaking "{}"..."#, text);

    festival_child.wait().expect("failed to wait for festival");

    Ok(())
}
