use web::ThreadPool;
use std::net::TcpListener;
use std::net::TcpStream;
use std::fs;
use std::io::{Read, Write};
use percent_encoding::percent_decode_str;

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

    match req.parse(&buffer) {
	Ok(status) => {
	    if status.is_partial() {
		println!("todo: handle http parse partial {:?}", status);
		return
	    }
	},
	Err(e) => {
	    println!("parse http error {}", e);
	    return
	},
    }

    let uris: Vec<&str> = req.path.unwrap().split('?').collect();
    let path = uris[0];

    let value = if uris.len() == 2 {
	let namevalue: Vec<&str> = uris[1].split("=").collect();
	if namevalue.len() == 2 && namevalue[0] == "name" {
	    Some(namevalue[1])
	} else {
	    None
	}
    } else {
	None
    };

    let filename = 
    if path.ends_with(".md") {
	"../generate/post".to_string() + &path.replace(".md", ".html")
    } else if path == "/category" && value.is_some() {
	if let Ok(val) = percent_decode_str(value.unwrap()).decode_utf8() {
	    "../generate/category/".to_string() + val.as_ref()
	} else {
	    "not_found".to_string()
	}
    } else if path == "/tags" && value.is_some() {
	if let Ok(val) = percent_decode_str(value.unwrap()).decode_utf8() {
	    "../generate/tags/".to_string() + val.as_ref()
	} else {
	    "not_found".to_string()
	}
    } else if path == "/" {
	 "../generate/home.html".to_string()
    } else if path == "/index" {
	 "../generate/index.html".to_string()
    } else if path == "/project" {
	"../generate/project.html".to_string()
    } else if path == "/about" {
	"../generate/about.html".to_string()
    } else if path == "/feed.atom" {
	"not_found".to_string() // TODO
    } else {
	"not_found".to_string()
    };

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
