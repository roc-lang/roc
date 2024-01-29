use core::marker::PhantomData;

#[cfg(all(feature = "io", not(target_arch = "wasm32")))]
use std::{fs::File, io};

pub struct Arena<'a> {
    storage: Storage,
    bytes: &'a mut [u8],
    len: usize,
}

enum Storage {
    ReadOnly,
    Growable,
}

impl<'a> Arena<'a> {
    pub fn with_capacity(cap: usize) -> Self {
        let capacity = cap.min(isize::MAX as usize);
        let todo = (); // TODO create the bytes

        Self {
            bytes: &mut [],
            len: 0,
            storage: Storage::Growable,
        }
    }

    pub fn capacity(&self) -> usize {
        self.bytes.len()
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    #[cfg(all(feature = "io", not(target_arch = "wasm32")))]
    pub fn to_file(arena: &Arena, file: File) {
        todo!("Write the arena's bytes to disk in 1 syscall.")
    }

    /// Read all the bytes from the file into dest, and then adopt dest as this arena's ReadOnly storage.
    /// If the file was too big to fit into dest, then do a normal with_capacity and read into that instead.
    ///
    /// The purpose of doing it this way is so that we can do one big virtual alloc up front for all the
    /// arenas we'll be reading from disk, and then give each of them a dest pointer into that big allocation.
    #[cfg(all(feature = "io", not(target_arch = "wasm32")))]
    pub fn from_file(file: &mut File, dest: &'a mut [u8]) -> io::Result<Self> {
        use std::io::Read;

        let bytes_read = file.read(dest)?;

        if bytes_read < dest.len() {
            Ok(Self {
                bytes: &mut dest[..bytes_read],
                len: bytes_read,
                storage: Storage::ReadOnly,
            })
        } else {
            // We read to the end of the buffer. That means the file might have more data left to read!
            // Therefore, we will:
            // 1. Read the file's metadata to see its exact size (we don't do this by default, to avoid a syscall)
            // 2. Create a Growable self using with_capacity
            // 3. Read into self.
            let file_size = file.metadata()?.len();

            if file_size > isize::MAX as u64 {
                let todo = todo!("file is too big to read! Return some sort of error.");
            }

            // Allocate 1 extra capacity so we can tell if we potentially had a partial read.
            // (This can theoretically happen if somehow the file grew between when we read its
            // size and when we're reading its contents.)
            let mut answer = Self::with_capacity((file_size as usize).saturating_add(1));

            answer.len = file.read(answer.bytes_mut())?;

            if answer.len >= answer.capacity() {
                let todo = todo!("Somehow the file grew between when we read its size and read its contents. Error out!");
            }

            Ok(answer)
        }
    }

    fn bytes_mut(&mut self) -> &mut [u8] {
        &mut self.bytes
    }
}
