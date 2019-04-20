#[macro_use]
extern crate log;
extern crate env_logger;
extern crate regex;

use regex::Regex;
use std::result::Result;
fn main () {
	let text = r#"(begin (+ 1 2 (+ 3 4 5) 6))"#;
	println!("text = {:?}", text);

	env_logger::init().unwrap();
	let mut o = InPort::new(text);
	println!("read o: {:?}", read(&mut o))
}

fn read(in_port: &mut InPort) -> Result<Option<AST>, &'static str> {

	let token1 = in_port.next_token();
	match token1 {
		Some(s) => read_ahead(in_port, s),
		None => Ok(None),
	}
}

fn read_ahead(in_port: &mut InPort, token: String) -> Result<Option<AST>, &'static str> {
	if token == "(" {
		let mut L = vec![];
		loop {
			match in_port.next_token() {
				Some(s) => {
					if s == ")" {
						return Ok(Some(AST::Children(L)))
					} else {
						match read_ahead( in_port, s) {
							Ok(Some(ast)) => L.push(ast),
							Ok(None) => {},
							_ => { return Err("EOF") }
						}
					}
				}
				None => {},
			}
		}
		unreachable!();
	} else if token == ")" {
		Err("unexpected )")
	} else {
		Ok(Some(atom(&token)))
	}
}


fn atom(token: &str) -> AST {
	let to_int = token.parse::<i64>();
	let to_float = token.parse::<f64>();

	if to_int.is_ok() {
		AST::Integer(to_int.unwrap_or_default())
	} else if to_float.is_ok() {
		AST::Float(to_float.unwrap_or_default())
	} else {
		AST::Symbol(token.to_string())
	}
}

#[derive(Debug)]
#[derive(PartialEq)]
pub struct ReadFromTokenResult {
	pub remain: Vec<String>,
	pub result: AST
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum AST {
	Integer(i64),
	Float(f64),
	Symbol(String),
	Children(Vec<AST>)
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

	fn next_token(&mut self) -> Option<String> {

		let re = Regex::new(r#"\s*(,@|[('`,)]|"(?:[\\].|[^\\"])*"|;.*|[^\s('"`,;)]*)(.*)"#).unwrap();

		if self.line.is_empty() {
			return None
		}

		while !self.line.is_empty() {
			let t = self.line.clone();
			let caps_option = re.captures(&t);
			match caps_option {
				Some(caps) => {
					let token = caps.get(1).map_or("", |m| m.as_str());
					self.line = String::from(caps.get(2).map_or("", |m| m.as_str()));
					self.token = Some(token.to_string());
					debug!("token = {:?}", token);
					return self.token.clone()
				},
				None => {}
			}
		}
		None
	}
}
