#[macro_use]
extern crate log;
extern crate env_logger;
extern crate regex;

use regex::Regex;
use std::result::Result;
fn main () {

	env_logger::init().unwrap();
	parse(r#"(begin (+ 1 2 (+ 3 4 5) 6))"#);
	parse(r#"(begin (print "hello world"))"#);
	parse(r#"(begin (print "hello world")"#);
	parse(r#"(begin (print "hello "world"))"#);
	parse(r#"(begin (print () "hello world"))"#);
}

fn parse(text: &str) -> Result<Option<AST>, &'static str> {
	debug!("text = {:?}", text);
	let mut o = InPort::new(text);
	let result =  read(&mut o);
	debug!("parse result: {:?}", result);
	result
}

fn read(in_port: &mut InPort) -> Result<Option<AST>, &'static str> {
	let token = in_port.next_token();
	match token {
		Ok(Some(s)) => read_ahead(in_port, s),
		Ok(None) => Ok(None),
		Err(e) => Err(e),
		_ => Err("unknown status")
	}
}

fn read_ahead(in_port: &mut InPort, token: String) -> Result<Option<AST>, &'static str> {
	if token == "(" {
		let mut L = vec![];
		while let Ok(Some(s)) = in_port.next_token() {
			if s == ")" {
				return Ok(Some(AST::Children(L)))
			} else {
				match read_ahead( in_port, s) {
					Ok(Some(ast)) => L.push(ast),
					Ok(None) => { debug!("read_ahead NONE") },
					_ => { return Err("syntax error") }
				}
			}
		}
		Err("syntax error")
	} else if token == ")" {
		Err("unexpected )")
	} else {
		Ok(Some(atom(&token)))
	}
}

fn atom(token: &str) -> AST {
	let to_int = token.parse::<i64>();
	let to_float = token.parse::<f64>();

	let re = Regex::new(r#"^"(.+)?"$"#).unwrap();

	debug!("atom token = {:?}", token);
	if let Some(caps) = re.captures(&token) {
		debug!("detect string = {:?}", caps);
		AST::String(caps.get(1).map_or("", |m| m.as_str()).to_string())
	} else if to_int.is_ok() {
		AST::Integer(to_int.unwrap_or_default())
	} else if to_float.is_ok() {
		AST::Float(to_float.unwrap_or_default())
	} else {
		AST::Symbol(token.to_string())
	}
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum AST {
	Integer(i64),
	Float(f64),
	String(String),
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

	fn next_token(&mut self) -> Result<Option<String>, &'static str> {

		let re = Regex::new(r#"\s*(,@|[('`,)]|"(?:[\\].|[^\\"])*"|;.*|[^\s('"`,;)]*)(.*)"#).unwrap();

		while !self.line.is_empty() {
			let t = self.line.clone();
			let caps_option = re.captures(&t);
			match caps_option {
				Some(caps) => {
					let token = caps.get(1).map_or("", |m| m.as_str()).to_string();
					let line = String::from(caps.get(2).map_or("", |m| m.as_str()));

					if token.is_empty() {
						debug!("caps 1 = {:?} token = {:?}", caps.get(1), token);
						return Err("syntax error");
					}

					self.token = Some(token.clone());
					self.line = line.clone();

					debug!("token = {:?} line = {:?}", token, line);
					return Ok(self.token.clone())
				},
				None => return Ok(None)
			}
		}
		Ok(None)
	}
}
