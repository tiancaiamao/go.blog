use std::thread;
use std::sync::mpsc;
use std::sync::{Arc, Mutex};


type Job = Box<dyn FnOnce() + Send + 'static>;

struct Worker {
    id: usize,
    thread: Option<thread::JoinHandle<()>>,
}

impl Worker {
    fn new(id: usize, rx: Arc<Mutex<mpsc::Receiver<Job>>>) -> Worker {
	let t = thread::spawn(move || loop {
	    let message = rx.lock().unwrap().recv();

	    match message {
		Ok(job) => {
		    println!("Worker {} got a job; executing.", id);
		    job();
		}
		Err(_) => {
                    println!("Worker {} disconnected; shutting down.", id);
		    break;
		}
	    }
	});
	Worker {
	    id: id,
	    thread: Some(t),
	}
    }
}

pub struct ThreadPool {
    workers: Vec<Worker>,
    sender: Option<mpsc::Sender<Job>>,
}

impl ThreadPool {
    pub fn new(size: usize) -> ThreadPool {
	assert!(size > 0);

	let (tx, rx) = mpsc::channel();
	let receiver = Arc::new(Mutex::new(rx));

	let mut workers = Vec::with_capacity(size);
	for id in 0..size {
	    workers.push(Worker::new(id, Arc::clone(&receiver)))
	}

	ThreadPool {workers, sender: Some(tx)}
    }

    pub fn execute<F: FnOnce() + Send + 'static>(&self, f: F) {
	let job = Box::new(f);
	self.sender.as_ref().unwrap().send(job).unwrap();
    }
}

impl Drop for ThreadPool {
    fn drop(&mut self) {
	drop(self.sender.take());

	for worker in &mut self.workers {
	    println!("Shutting down worker {}", worker.id);

	    if let Some(thread) = worker.thread.take() {
		thread.join().unwrap();
	    }
	}
    }
}
