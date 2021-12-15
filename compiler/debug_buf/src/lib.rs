use std::{collections::{HashMap, HashSet}, ffi::c_void, path::PathBuf};

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct Address(u64);

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct FrameIndex(u32);

pub struct FlightRecorder {
    tmp_frame_buf: Vec<Address>,
    frames: Vec<(Address, FrameIndex)>,
    unique_frames: HashMap<(Address, FrameIndex), FrameIndex>,
    ranges: Vec<(FrameIndex, usize, usize)>,
    text: std::string::String,
}

impl FlightRecorder {
    pub fn new() -> Self {
        FlightRecorder {
            tmp_frame_buf: Vec::new(),
            frames: Vec::new(),
            unique_frames: HashMap::new(),
            ranges: Vec::new(),
            text: std::string::String::new(),
        }
    }

    pub fn push(&mut self, ch: char) {
        self.text.push(ch);
    }

    pub fn push_str(&mut self, s: &str) {
        self.text.push_str(s);
    }

    pub fn len(&self) -> usize {
        self.text.len()
    }

    pub fn take_sample(&mut self, offset: usize, len: usize) {
        assert!(offset <= self.text.len());
        assert!(offset + len <= self.text.len());
        let top = self.capture_backtrace();
        self.ranges.push((top, offset, len));
    }

    fn capture_backtrace(&mut self) -> FrameIndex {
        self.tmp_frame_buf.clear();
        backtrace::trace(|frame| {
            let ip = Address(frame.ip() as u64);
            self.tmp_frame_buf.push(ip);
            true // keep going to the next frame
        });

        let mut last_frame = FrameIndex(u32::MAX);
        for ip in self.tmp_frame_buf.drain(..).rev() {
            let frames = &mut self.frames;
            
            last_frame = *self.unique_frames.entry((ip, last_frame)).or_insert_with(|| {
                let next_frame_id = FrameIndex(frames.len() as u32);
                frames.push((ip, last_frame));
                next_frame_id
            });
        }

        last_frame
    }

    pub fn serialize(&self) -> Vec<u8> {
        let addresses = self.frames.iter().map(|(addr, _)| *addr).collect::<HashSet<_>>();
        #[derive(serde::Serialize)]
        struct Frame {
            addr: Option<u64>,
            name: Option<std::string::String>,
            filename: Option<std::path::PathBuf>,
            lineno: Option<u32>,
            colno: Option<u32>,
        }
        let addrs = addresses.into_iter().map(|addr| {
            let mut res = Vec::new();
            backtrace::resolve(addr.0 as *mut c_void, |symbol| {
                res.push(Frame {
                    addr: symbol.addr().map(|a| a as u64),
                    name: symbol.name().map(|n| n.to_string()),
                    filename: symbol.filename().map(|n| n.to_owned()),
                    lineno: symbol.lineno(),
                    colno: symbol.colno()
                });
            });
            (addr.0, res)
        }).collect::<HashMap<_, _>>();

        #[derive(serde::Serialize)]
        struct Encoded {
            addrs: HashMap<u64, Vec<Frame>>,
            frames: Vec<(u64, u32)>,
            ranges: Vec<(u32, usize, usize)>,
            text: std::string::String,
        }

        let encoded = Encoded {
            addrs,
            frames: self.frames.iter().map(|(a, f)| (a.0, f.0)).collect(),
            ranges: self.ranges.iter().map(|(f, offset, len)| (f.0, *offset, *len)).collect(),
            text: self.text.clone(),
        };

        serde_json::to_string(&encoded).unwrap().into_bytes()
    }
}

impl Drop for FlightRecorder {
    fn drop(&mut self) {
        // Write to a file if possible
        if let Ok(dir) = std::env::var("ROC_DEBUG_FMT_DIR") {
            use blake2::Blake2bVar;
            use blake2::digest::{Update, VariableOutput};

            let mut hasher = Blake2bVar::new(16).unwrap();
            hasher.update(self.text.as_bytes());
            // TODO: figure out a better naming scheme
            hasher.update(&std::time::SystemTime::now().duration_since(std::time::SystemTime::UNIX_EPOCH).unwrap().as_micros().to_le_bytes());
            let mut buf = [0u8; 16];
            hasher.finalize_variable(&mut buf).unwrap();
            
            let mut filename = base32::encode(base32::Alphabet::RFC4648 { padding: false }, &buf);
            filename.push_str(".rocfmtdbg");
            let path = PathBuf::from(dir).join(filename);

            std::fs::write(&path, self.serialize()).unwrap();
            println!("Wrote recording to {}", std::fs::canonicalize(path).unwrap().display());
        }
    }
}
