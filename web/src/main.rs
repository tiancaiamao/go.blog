use web::ThreadPool;
use std::net::TcpListener;
use std::net::TcpStream;
use std::fs;
use std::str;
use std::io::{Read, Write};

fn main() {
    println!("Hello, world!");
    let listener = TcpListener::bind("0.0.0.0:7878").unwrap();
    let pool = ThreadPool::new(4);

    for stream in listener.incoming() {
        let stream = stream.unwrap();

	pool.execute(|| {
            handle_connection(stream);
        });
    }
}

fn handle_connection(mut stream: TcpStream) {
    let mut buffer = [0; 1024];
    stream.read(&mut buffer).unwrap();

    let mut headers = [httparse::EMPTY_HEADER; 64];
    let mut req = httparse::Request::new(&mut headers);

    // let buf = b"GET /index.html HTTP/1.1\r\nHost";
    // assert!(req.parse(buf)?.is_partial());

    // // a partial request, so we try again once we have more data
    // let buf = b"GET /index.html HTTP/1.1\r\nHost: example.domain\r\n\r\n";
    // assert!(req.parse(buf)?.is_complete());

    let sz;
    match req.parse(&buffer) {
	Ok(status) => {
	    if status.is_partial() {
		println!("todo: handle http parse partial {:?}", status);
		return
	    }
	    sz = status.unwrap();
	},
	Err(e) => {
	    println!("parse http error {}", e);
	    return
	},
    }

    let s = str::from_utf8(&buffer[0..sz]).unwrap();
    // first line GET / HTTP/1.1
    let header = s.lines().next().unwrap();
    let mut tmp = header.split_whitespace();
    let _method = tmp.next().unwrap();
    let uri = tmp.next().unwrap();
    let _version = tmp.next().unwrap();
    
    println!("read head == {:?}", s);
    println!("read head == {}, {}, {}", _method, uri, _version);

    let filename = "../generate".to_string() + &uri.replace(".md", ".html");
    // if let Err(e) = fs.exists(&filename) {
    // 	    println!("request path not exist {}", filename);
    // 	    return
    // }

    // let (status_line, filename) = ("HTTP/1.1 404 NOT FOUND", "404.html");
    // if fs::exists(&filename).is_ok() {
    // 	(status_line, filename) = ("HTTP/1.1 200 OK", filename);
    // }

    let (status_line, contents) = match fs::read_to_string(filename) {
	Ok(content) => {
	    ("HTTP/1.1 200 OK", content)
	},
	Err(_) => {
	    ("HTTP/1.1 404 NOT FOUND", fs::read_to_string("404.html").unwrap())
	},
    };

    let response = format!(
        "{}\r\nContent-Length: {}\r\n\r\n{}",
        status_line,
        contents.len(),
        contents
    );

    stream.write_all(response.as_bytes()).unwrap();
    stream.flush().unwrap();
}
