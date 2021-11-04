use rodio::{Decoder, OutputStream, Sink};
use std::fs::File;
use std::io::BufReader;

pub(crate) fn play_sound(sound_path_str: &str) {
    let out_stream_res = OutputStream::try_default();

    match out_stream_res {
        Ok((_, out_stream_handle)) => match Sink::try_new(&out_stream_handle) {
            Ok(sink) => match File::open(sound_path_str) {
                Ok(file) => {
                    let reader = BufReader::new(file);

                    match Decoder::new(reader) {
                        Ok(decoder) => {
                            sink.append(decoder);
                            sink.sleep_until_end();
                        }
                        Err(e) => {
                            println!("Failed to create Decoder from BufReader from sound file at {}. Error message: {:?}", sound_path_str, e);
                        }
                    }
                }
                Err(e) => {
                    println!(
                        "Failed to open sound file at {}. Error message: {}",
                        sound_path_str, e
                    );
                }
            },
            Err(e) => {
                println!(
                    "Failed to create Sink to play sound. Error message: {:?}",
                    e
                );
            }
        },
        Err(e) => {
            println!(
                "Failed to create OutputStream to play sound. Error message: {:?}",
                e
            );
        }
    }
}
