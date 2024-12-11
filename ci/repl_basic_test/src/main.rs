use rexpect::error::Error;
use rexpect::session::PtyReplSession;
use rexpect::spawn;
use std::thread;
use std::time::Duration;

fn roc_repl_session() -> Result<PtyReplSession, Error> {
    let roc_repl = PtyReplSession {
        echo_on: false,
        prompt: "\u{1b}[0K\u{1b}[1;36m»\u{1b}[0m ".to_string(),
        pty_session: spawn("./roc repl", Some(7000))?,
        quit_command: None,
    };
    thread::sleep(Duration::from_secs(1));
    Ok(roc_repl)
}

fn main() -> Result<(), Error> {
    let mut repl = roc_repl_session()?;

    repl.exp_regex(".*roc repl.*")?;
    repl.send_line("1+1")?;

    thread::sleep(Duration::from_secs(1));

    match repl.exp_regex(r".*2\u{1b}\[1;32m : \u{1b}\[0mNum *.*") {
        // 2 : Num
        Ok(_) => {
            println!("Expected output received.");
            return Ok(());
        }
        Err(_) => {
            eprintln!("\nError: output was different from expected value.");
            std::process::exit(1);
        }
    }
}
