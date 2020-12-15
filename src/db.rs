//! Custom storage engine, prioritizing safety over speed.
//! I was planning on using SQLite, but this is more fun.
use ::std::fs::{File, OpenOptions};
use ::std::path::Path;
use ::std::io;

// I'd love to only support Linux,
// but I specifically have to support Windows.
// Gonna be so fun to roll my own storage engine.

// Servers
// Categories
// Channels

/// Offset into database file.
struct Offset(usize);

struct Server {
    id: u64,
}

struct Category {
    id: u64,
}

struct Channel {
    id: u64,
}

struct Header {
    
}

struct DB {
    file: File,
    header: Header,
}
impl DB {
    fn open(path: &Path) -> io::Result<Self> {
        let mut options = OpenOptions::new();
        let file = options.open(path)?;
        Ok(Self {
            file,
            free: todo!(), 
        })
    }
}

pub(crate) async fn initialize() -> Result<(), ()> {
    todo!("custom storage engine initialization")
}
pub(crate) async fn shutdown() {}
