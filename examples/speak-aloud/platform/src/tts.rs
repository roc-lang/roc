use tts::{Backends, Tts};

pub fn speak(text: &str) {
    let backend = Backends::SpeechDispatcher;

    let mut tts = Tts::new(backend).expect("Failed to initialize TTS");

    let utterance = tts
        .speak(text, false)
        .expect(&format!(r#"Failed to speak "{}""#, text));

    // TODO: this will be changed to be async
    std::thread::sleep(std::time::Duration::from_secs(5));
}
