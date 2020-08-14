use foretry::{async_ltry, async_try, brk, cont};
use log::{info, trace, warn};
use std::io;
use std::path::{Path, PathBuf};
use std::string::FromUtf8Error;
use thiserror::Error;
use tokio::{io::AsyncReadExt, net::UnixListener, stream::StreamExt};

/// Wrapper for `UnixListener` that destroys the socket file
/// when it's finished.
struct DropSocket {
    path: PathBuf,
    listener: UnixListener,
}
impl DropSocket {
    fn bind(path: impl AsRef<Path>) -> Result<Self, io::Error> {
        let path = path.as_ref().to_owned();
        UnixListener::bind(&path).map(|listener| DropSocket { path, listener })
    }
}
impl Drop for DropSocket {
    fn drop(&mut self) {
        match std::fs::remove_file(&self.path) {
            Ok(_) => (),
            Err(e) => warn!("error destroying socket: {}", e),
        }
    }
}
impl std::ops::Deref for DropSocket {
    type Target = UnixListener;
    fn deref(&self) -> &Self::Target {
        &self.listener
    }
}
impl std::ops::DerefMut for DropSocket {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.listener
    }
}

#[derive(Error, Debug)]
enum InternalError {
    #[error("io: {0}")]
    IoError(#[from] io::Error),
    #[error("utf8: {0}")]
    FromUtf8(#[from] FromUtf8Error),
}

// Shutdown Behavior:
//  1. Stop accepting new connections to the control socket.
//  2. Finalize all existing connections to the control socket.
//  3. Shutdown all Discord Shards.
//  4. Close SQLx connection pool.
//    a. Stop accepting new connections.
//    b. Finalize all existing connections.
//  5. Exit the process.
pub(crate) async fn control_loop<T: AsRef<Path>>(p: T) -> Result<(), io::Error> {
    let mut listener = DropSocket::bind(p)?;
    println!("Heya");
    while let Some(stream) = listener.next().await {
        trace!("new connection to control socket");
        tokio::spawn(async move {
            async_try!{(), InternalError | {
                let mut buf = Vec::new();
                let num_bytes = stream?.read_buf(&mut buf).await?;
                let bufstr = String::from_utf8(buf)?;
                println!("received msg: {}", bufstr);
                if num_bytes == 0 {
                    trace!("reached end of stream from control socket connection");
                }
            } catch (e) {
                warn!("control socket error: {}", e);
            }}
        });
    }

    println!("Closed control socket loop");
    Ok(())
}
