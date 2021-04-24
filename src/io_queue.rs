use std::{
    io::{Error, ErrorKind, Write},
    mem::take,
    path::{Path, PathBuf},
    sync::mpsc::{channel, sync_channel, Receiver, Sender, SyncSender},
    thread::{spawn, JoinHandle},
};

enum Command {
    File(PathBuf, Vec<u8>),
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

    fn run_thread(self) -> Result<(), ()> {
        loop {
            match self.queue.recv() {
                Ok(Command::File(path, buffer)) => {
                    if let Err(e) = std::fs::write(path, buffer) {
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
        Ok(IORequest { controller: self, path: path.to_path_buf(), buffer: Vec::with_capacity(8192) })
    }

    pub fn write_file(&self, path: &Path, content: Vec<u8>) -> std::io::Result<()> {
        let _ = self.queue.send(Command::File(path.to_path_buf(), content));
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
}

impl<'a> Write for IORequest<'a> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.buffer.extend_from_slice(buf);
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
        let _ = self.controller.queue.send(Command::File(take(&mut self.path), take(&mut self.buffer)));
    }
}
