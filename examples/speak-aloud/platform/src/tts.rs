use futures::task::{LocalSpawn, LocalSpawnExt, Spawn};
use std::cell::RefCell;
use std::ops::DerefMut;
use std::pin::Pin;
use std::rc::Rc;
use std::{
    future::Future,
    task::{Context, Poll},
};
use tts::{Backends, Tts};

/// Trait to implement async support for [`Tts`].
trait TtsExt {
    fn speak_async<'a>(&mut self, text: &'a str, interrupt: bool) -> SpeakAsync<'a>;
}

impl TtsExt for Tts {
    fn speak_async<'a>(&mut self, text: &'a str, interrupt: bool) -> SpeakAsync<'a> {
        SpeakAsync {
            tts: self.clone(),
            text,
            interrupt,
            first_call: Rc::new(RefCell::new(true)),
        }
    }
}

/// Struct which allows awaiting an utterance.
pub struct SpeakAsync<'a> {
    tts: Tts,
    text: &'a str,
    interrupt: bool,
    first_call: Rc<RefCell<bool>>,
}

impl<'a> Future for SpeakAsync<'a> {
    type Output = Result<(), tts::Error>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // If this is our first time being polled, register the callback
        if *self.first_call.borrow() {
            let waker = cx.waker().to_owned();
            let first_call = self.first_call.clone();

            self.tts
                .on_utterance_end(Some(Box::new(move |_| {
                    *first_call.borrow_mut() = false;

                    // We want to wake by ref to avoid consuming waker,
                    // or cloning it
                    waker.wake_by_ref()
                })))
                .expect("Failed to register `on_utterance_end` callback");

            // We need to move these items out of `self` to avoid
            // having a mutable borrow in the same scope as
            // immutable borrows
            let text = self.text;
            let interrupt = self.interrupt;

            match self.tts.speak(text, interrupt) {
                Ok(_) => Poll::Pending,
                Err(err) => Poll::Ready(Err(err)),
            }
        }
        // Otherwise, this means we have been woken up by an
        // utterence ended event, and are therefore done
        else {
            Poll::Ready(Ok(()))
        }
    }
}

pub fn speak(text: &str) {
    // Specify the TTS backend that will be used
    #[cfg(target_os = "android")]
    let backend = Backends::Android;
    #[cfg(any(target_os = "macos", target_os = "ios"))]
    let backend = Backends::AvFoundation;
    #[cfg(target_os = "linux")]
    let backend = Backends::SpeechDispatcher;
    #[cfg(all(windows, feature = "tolk"))]
    let backend = Backends::Tolk;
    #[cfg(target_arch = "wasm32")]
    let backend = Backends::Web;
    #[cfg(windows)]
    let backend = Backends::WinRt;

    // Create Tts instance
    let mut tts = Tts::new(backend).expect("Failed to initialize TTS");

    // Speak text
    futures::executor::block_on(tts.speak_async(text, false))
        .unwrap_or_else(|_| panic!(r#"Failed to speak "{}""#, text));

    println!(r#"Done speaking "{}""#, text);
}
