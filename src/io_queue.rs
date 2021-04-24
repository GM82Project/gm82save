use std::{
    fs::File,
    io::{Error, ErrorKind, Write},
    mem::take,
    path::{Path, PathBuf},
    sync::mpsc::{channel, sync_channel, Receiver, Sender, SyncSender},
    thread::{spawn, JoinHandle},
};

const DEFAULT_CAPACITY: usize = 8192;
const SPLIT_WRITES: bool = false;

enum Command {
    Open(PathBuf, Vec<u8>),
    Append(Vec<u8>),
    Finish,
}

struct IOThread {
    queue: Receiver<Command>,
    error: SyncSender<Error>,
}

impl IOThread {
    fn new(queue: Receiver<Command>, error: SyncSender<Error>) -> Self {
        Self { queue, error }
    }

    fn open(path: PathBuf, buf: Vec<u8>) -> std::io::Result<File> {
        let mut f = File::create(path)?;
        f.write_all(&buf)?;
        Ok(f)
    }

    fn run_thread(self) -> Result<(), ()> {
        // i'd love to not have this epic code duplication but not totally sure how
        let mut file = match self.queue.recv() {
            Ok(Command::Open(path, buffer)) => match Self::open(path, buffer) {
                Ok(f) => f,
                Err(e) => {
                    let _ = self.error.send(e);
                    return Err(())
                },
            },
            Ok(Command::Finish) => return Ok(()),
            Ok(Command::Append(_)) => {
                let _ = self.error.send(Error::new(ErrorKind::Other, "no file open"));
                return Err(())
            },
            Err(_) => {
                let _ = self.error.send(ErrorKind::BrokenPipe.into());
                return Err(())
            },
        };
        loop {
            match self.queue.recv() {
                Ok(Command::Open(path, buffer)) => {
                    file = match Self::open(path, buffer) {
                        Ok(f) => f,
                        Err(e) => {
                            let _ = self.error.send(e);
                            return Err(())
                        },
                    };
                },
                Ok(Command::Append(buf)) => {
                    if let Err(e) = file.write_all(&buf) {
                        let _ = self.error.send(e);
                        return Err(())
                    }
                },
                Ok(Command::Finish) => return Ok(()),
                Err(_) => {
                    let _ = self.error.send(ErrorKind::BrokenPipe.into());
                    return Err(())
                },
            }
        }
    }
}

pub struct IOController {
    handle: JoinHandle<Result<(), ()>>,
    pub(self) queue: Sender<Command>,
    pub(self) error: Receiver<Error>,
}

impl IOController {
    pub fn new() -> Self {
        let (queue_tx, queue_rx) = channel();
        let (error_tx, error_rx) = sync_channel(0);
        Self { handle: spawn(move || IOThread::new(queue_rx, error_tx).run_thread()), queue: queue_tx, error: error_rx }
    }

    pub fn open_file(&self, path: &Path) -> std::io::Result<IORequest> {
        if let Ok(err) = self.error.try_recv() {
            return Err(err)
        }
        Ok(IORequest {
            controller: self,
            path: path.to_path_buf(),
            buffer: Vec::with_capacity(DEFAULT_CAPACITY),
            sent_first: false,
        })
    }

    pub fn write_file(&self, path: &Path, content: Vec<u8>) -> std::io::Result<()> {
        let _ = self.queue.send(Command::Open(path.to_path_buf(), content));
        if let Ok(err) = self.error.try_recv() {
            return Err(err)
        }
        Ok(())
    }

    pub fn finish(self) -> std::io::Result<()> {
        let _ = self.queue.send(Command::Finish);
        match self.handle.join() {
            Ok(Ok(())) => Ok(()),
            Ok(Err(())) => Err(self.error.recv().unwrap_or(ErrorKind::BrokenPipe.into())),
            Err(e) => std::panic::resume_unwind(e),
        }
    }
}

pub struct IORequest<'a> {
    controller: &'a IOController,
    path: PathBuf,
    buffer: Vec<u8>,
    sent_first: bool,
}

impl<'a> IORequest<'a> {
    fn send_buf(&mut self) {
        let _ = self.controller.queue.send(if self.sent_first {
            Command::Append(take(&mut self.buffer))
        } else {
            self.sent_first = true;
            // this will invalidate self.path but whatever
            Command::Open(take(&mut self.path), take(&mut self.buffer))
        });
    }
}

impl<'a> Write for IORequest<'a> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if SPLIT_WRITES {
            let leftover_cap = self.buffer.capacity() - self.buffer.len();
            if buf.len() > leftover_cap {
                self.buffer.extend_from_slice(&buf[..leftover_cap]);
                self.send_buf();
                self.buffer.reserve(DEFAULT_CAPACITY);
                self.buffer.extend_from_slice(&buf[leftover_cap..]);
            } else {
                self.buffer.extend_from_slice(buf);
            }
        } else {
            if self.buffer.len() + buf.len() > self.buffer.capacity() {
                self.buffer.reserve(buf.len() - self.buffer.len() + DEFAULT_CAPACITY);
            }
            self.buffer.extend_from_slice(buf);
        }
        if let Ok(err) = self.controller.error.try_recv() {
            return Err(err)
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl<'a> Drop for IORequest<'a> {
    fn drop(&mut self) {
        self.send_buf();
    }
}
