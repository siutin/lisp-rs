extern crate regex;

use regex::Regex;
use std::result::Result;
fn main () {
//	let text = r#"(begin (print "hello world"))"#;
	let text = r#"(begin (+ 1 2 (+ 3 4 5) 6))"#;
	println!("text = {:?}", text);

	let result = in_port(text);
	println!("result: {:?}", result)
}

fn in_port(text: &str) -> Result<String, &'static str> {

	let re = Regex::new(r#"\s*(,@|[('`,)]|"(?:[\\].|[^\\"])*"|;.*|[^\s('"`,;)]*)(.*)"#).unwrap();

	let mut line = String::from(text);

	if line.is_empty() {
		return Err(&"empty line")
	}

	while !line.is_empty() {
		let t = line.clone();
		let caps_option = re.captures(&t);
		match caps_option {
			Some(caps) => {
				let token = caps.get(1).map_or("", |m| m.as_str());
				line = String::from(caps.get(2).map_or("", |m| m.as_str()));
				println!(" 1 = {}", token);
			},
			None => {}
		}
	}

	Ok("GOOD".to_string())
}