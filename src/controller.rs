//! A better interface for controlling the bot.
//! The threat model for this interface is as follows:
//! We only accept local connections, and completely trust
//! every user on the machine we're running on.
use ::std::net::Ipv4Addr;
use ::std::io;
use ::std::borrow::Cow;
use ::tokio::net::{TcpListener, TcpStream, tcp};
use ::tokio::io::{AsyncReadExt, AsyncWriteExt};
use ::tokio::sync::watch;
use ::serenity::client::Context as BotContext;
use ::serenity::model::gateway::Ready as BotReady;

pub enum Never {}

// TODO: keep a listing of connections to the control interface?
enum ConnectionType {
    Http,
    AnsiCli,
}

/// HTTP stuff for serving the Web interface, which
/// is bundled into the binary for Release builds.
mod http {
    use ::std::borrow::Cow;
    use ::rust_embed::RustEmbed;

    #[derive(RustEmbed)]
    #[folder = "csite/"]
    struct Asset;

    fn response(headers: &[&[u8]], body: &[u8]) -> Vec<u8> {
        let mut buf: Vec<u8> = headers.join(b"\r\n".as_ref());
        // Add newline after last header, then empty line before optional body.
        buf.extend(b"\r\n\r\n");
        buf.extend(body);

        buf
    }

    fn file_response(filename: &str, content_type: &str) -> Cow<'static, [u8]> {
        Cow::Owned(response(&[b"HTTP/1.1 200 OK",
                              format!("Content-Type: {}", content_type).as_bytes(),
                              b"Connection: Closed"],
                            &*Asset::get(filename).unwrap()))
    }

    pub fn index_file_response() -> Cow<'static, [u8]> {
        let response: Vec<u8> = response(&[b"HTTP/1.1 200 OK",
                                           b"Content-Type: text/html",
                                           b"Connection: Closed"],
                                         &*Asset::get("index.html").unwrap());
        Cow::Owned(response)
    }

    pub fn script_file_response() -> Cow<'static, [u8]> {
        file_response("controller.js", "text/javascript")
    }
}

// We use this to avoid needing to wait on connecting to Discord to
// start up the controller interface.
/// An abstraction for waiting on the result of a thing (sortof) by reference.
/// Like a [`oneshot`](::tokio::sync::oneshot) channel,
/// except that there can be multiple receivers.
pub(crate) struct Jar<T> {
    receiver: watch::Receiver<Option<T>>,
}
pub(crate) struct JarSender<T> {
    sender: watch::Sender<Option<T>>,
}
pub(crate) struct JarRef<'a, T> {
    borrow: watch::Ref<'a, Option<T>>,
}
impl<'a, T> ::core::ops::Deref for JarRef<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.borrow.as_ref().unwrap()
    }
}
impl<T> Jar<T> {
    pub(crate) fn new() -> (JarSender<T>, Self) {
        let (sender, receiver) = watch::channel(None);
        (JarSender { sender }, Self { receiver })
    }
    pub(crate) fn waiter(&self) -> Jar<T> {
        Self { receiver: self.receiver.clone() }
    }
    pub(crate) async fn borrow(&mut self) -> JarRef<'_, T> {
        while self.receiver.borrow().is_none() {
            println!("Waiting...");
            self.receiver.changed().await.unwrap()
        }
        JarRef { borrow: self.receiver.borrow() }
    }
}
impl<T> JarSender<T> {
    pub(crate) fn fill(self, val: T) {
        self.sender.send(Some(val)).map_err(|_| ()).unwrap();
    }
}

pub(crate) async fn listen(port: u16, bot_context: Jar<(BotContext, BotReady)>) -> Result<Never, io::Error> {
    let listener = TcpListener::bind((Ipv4Addr::new(127, 0, 0, 1), port)).await?;

    loop {
        // TODO: handle non-fatal errors
        let (mut stream, _) = listener.accept().await?;
        // lol ansi escape code to move the cursor up
        // stream.write(b"\x1b[A").await;
        let bot_context = bot_context.waiter();
        ::tokio::spawn(async move {
            // Note that our buffer must be at least 4K, due to the below limit for HTTP.
            let mut buf: Vec<u8> = vec![0; 8192];
            match stream.read(&mut buf).await {
                Ok(amt) => {
                    use ::httparse::Status;
                    dbg!((amt, String::from_utf8_lossy(&buf[..amt])));
                    let mut headers = [httparse::EMPTY_HEADER; 16];
                    let mut req = httparse::Request::new(&mut headers);

                    // We limit the length of the collection of HTTP headers to 4K.
                    match req.parse(&buf[..4096]) {
                        Ok(Status::Complete(offset_body)) => {
                            dbg!(offset_body);
                            dbg!(req.path);
                            let response = match req.path.unwrap() {
                                "/" => http::index_file_response(),
                                "/controller.js" => http::script_file_response(),
                                _ => Cow::Borrowed(b"HTTP/1.1 404 Not Found\r\n\r\n".as_ref()),
                            };

                            stream.write(&response).await.unwrap();
                        },
                        Ok(Status::Partial) => {
                            stream.write(b"HTTP/1.1 413 Entity Too Large\r\n\r\n")
                                .await.unwrap();
                        },
                        Err(::httparse::Error::Token) => {
                            // To use a non-HTTP protocol for connecting to mbot,
                            // your protocol must have a header that isn't valid HTTP.
                            // (Though it could just as easily use an HTTP header and
                            // have a special magic path for changing to a different protocol.)
                            match &buf[..amt] {
                                [b'T', b'E', b'R', b'M', b':', b' ', rest @ ..] => {
                                    match rest.iter().enumerate().find(|(_i, b)| **b == b'\n') {
                                        Some((line_end, _)) => {
                                            stream.write(b"Hello, terminal user!\n")
                                                .await.unwrap();
                                            let term = rest.as_ptr() as usize - buf.as_ptr() as usize;
                                            let term = (term, term + line_end);
                                            handle_cli_connection(buf, amt, term, stream, bot_context).await;
                                        },
                                        None => todo!("expected newline")
                                    }
                                },
                                _ => todo!("unknown protocol")
                            }
                        }
                        Err(err) => { dbg!(err); },
                    }
                },
                Err(e) => { dbg!(e); },
            }
        });
    }
}

/// A reimplementation of the CLI command loop, over a TCP connection.
async fn handle_cli_connection(mut buf: Vec<u8>, end: usize,
                               (term_start, term_end): (usize, usize),
                               mut stream: TcpStream,
                               mut ready_jar: Jar<(BotContext, BotReady)>) {
    use ::serenity::model::id::ChannelId;
    // TODO: do things based on the kind of terminal in use?
    let terminal = Vec::from(&buf[term_start..term_end]);
    let cursor = term_end + 1;

    struct LineReader<'a> {
        buf: &'a mut Vec<u8>,
        cursor: usize,
        end: usize,
        stream: tcp::ReadHalf<'a>,
    }
    impl<'a> LineReader<'a> {
        fn new(buf: &'a mut Vec<u8>, cursor: usize, end: usize, stream: tcp::ReadHalf<'a>) -> Self {
            Self { buf, cursor, end, stream }
        }
        async fn next(&mut self) -> Result<Option<&[u8]>, ()> {
            match self.buf[self.cursor..self.end].iter().enumerate().find(|(_i, b)| **b == b'\n') {
                Some((line_end, _)) => {
                    let line = &self.buf[self.cursor.. self.cursor + line_end];
                    self.cursor += line_end + 1;
                    Ok(Some(line))
                },
                None => {
                    match self.stream.read(&mut self.buf[self.end..]).await {
                        // TcpStream promises to always return more than 0 bytes until close
                        Ok(amt) if amt > 0 => {
                            self.end += amt;
                            Ok(None)
                        },
                        // TODO: examine non-fatal or non-closed error cases
                        Ok(_) | Err(_) => {
                            Err(())
                        }
                    }
                }
            }
        }
    }
    let (read_half, mut write_half) = stream.split();
    let mut reader = LineReader::new(&mut buf, cursor, end, read_half);

    let mut current_channel: Option<ChannelId> = None;

    macro_rules! bot_ctx {
        () => {
            ready_jar.borrow().await.0
        }
    }
    macro_rules! prompt {
        () => {
            write_half.write(b"mbot~$ ").await.unwrap();
        }
    }
    prompt!();
    while let Ok(status) = reader.next().await {
        use crate::cmd::Command;
        match status {
            Some(line) => {
                let cmd = match crate::cmd::parse_command(::core::str::from_utf8(line).unwrap()) {
                    Ok(x) => x,
                    Err(_) => {
                        write_half.write(b"[ERROR]: Invalid Command\n").await.unwrap();
                        prompt!();
                        continue
                    }
                };
                match cmd {
                    Command::SayImplicitChannel(text) => {
                        if let Some(current_channel) = current_channel {
                            match current_channel.say(&bot_ctx!().http, text).await {
                                Ok(_) => (),
                                Err(e) => {
                                    write_half.write(format!("[ERROR]: {}\n", e).as_bytes()).await.unwrap();
                                },
                            }
                        } else {
                            write_half.write(b"[ERROR]: No channel selected!\n").await.unwrap();
                        }
                    },
                    Command::SelectChannel(id) => {
                        current_channel = Some(id);
                    },
                    Command::ListGuilds => {
                        let (ctx, ready) = &*ready_jar.borrow().await;
                        let guilds = ready.user.guilds(&ctx.http).await;
                        match guilds {
                            Ok(x) => {
                                let list = x.into_iter().map(|x| x.name).collect::<Vec<_>>();
                                write_half.write(format!("{:?}\n", list).as_bytes()).await.unwrap();
                            },
                            Err(e) => {
                                write_half.write(format!("[ERROR]: {:#?}", e).as_bytes()).await.unwrap();
                            }
                        }
                    },
                    #[cfg(feature = "plotting")]
                    Command::ReloadPlotter => {
                        ::tokio::task::spawn_blocking(crate::dist::Plotter::reload).await.unwrap();
                    }
                    Command::Shutdown => {
                        write_half.write(b"Shutting down...\n").await.unwrap();
                        crate::shutdown().await
                    },
                    // _ => {
                    //     write_half.write(b"[ERROR]: Unimplemented Command\n").await.unwrap();
                    // }
                }
                println!("{}", String::from_utf8_lossy(line));
            },
            None => continue,
        }
        // TODO: command to suppress prompt during script execution?
        prompt!();
    }
    println!("Done.");
}
