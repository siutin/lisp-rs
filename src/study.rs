extern crate regex;

use regex::Regex;
use std::result::Result;
fn main () {
	let text = r#"(begin (+ 1 2 (+ 3 4 5) 6))"#;
	println!("text = {:?}", text);

	let mut o = InPort::new(text);
	match o.next_token() {
		Ok(Some(s)) => println!("next token = {:?}", s),
		_ => 	println!("no more next token")
	}
}

struct InPort {
	token: Option<String>,
	line: String
}

impl InPort {
	fn new (text: &str) -> InPort {
		InPort {
			token: None,
			line: String::from(text)
		}
	}
	fn token(&self) -> Option<String> {
		self.token.clone()
	}

	fn next_token(&mut self) -> Result<Option<String>, &'static str> {

		let re = Regex::new(r#"\s*(,@|[('`,)]|"(?:[\\].|[^\\"])*"|;.*|[^\s('"`,;)]*)(.*)"#).unwrap();

		if self.line.is_empty() {
			return Err(&"empty line")
		}

		while !self.line.is_empty() {
			let t = self.line.clone();
			let caps_option = re.captures(&t);
			match caps_option {
				Some(caps) => {
					let token = caps.get(1).map_or("", |m| m.as_str());
					self.line = String::from(caps.get(2).map_or("", |m| m.as_str()));
					self.token = Some(token.to_string());
					return Ok(self.token.clone())
				},
				None => {}
			}
		}
		Ok(None)
	}
}
