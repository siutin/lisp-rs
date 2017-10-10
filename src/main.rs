#[derive(Debug)]
enum AST {
	Fixnum(u32),
	Float(f64),
	Symbol(String),
	Children(Vec<AST>)
}

fn main() {
	// println!("Hello, world!");
	let program = "(begin (define r 10) (* pi (* r r)))";
	println!("program: {}", program);
	let tokens = tokenize(program);
	println!("tokens: {:?}", tokens);
	let ast = read_from_tokens(tokens);
	println!("ast: {:?}", ast);
}

fn tokenize(program: &str) -> Vec<String>
{
	let iterator = program.chars();
	let iterator1 = iterator.clone();
	let mut iterator2 = iterator1.clone();

	let count = iterator1.count();
	let mut vec:Vec<char> = Vec::with_capacity(count);

	// println!("{:?}", iterator2);

	loop {
		match iterator2.next() {
			Some(x) => {
				if x == '(' {
					vec.push(' ');
					vec.push('(');
					vec.push(' ');
				} else if x == ')' {
					vec.push(' ');
					vec.push(')');
					vec.push(' ');
				} else {
					vec.push(x);
				}
			},
			None => { break; }
		}
	}

	// println!("vec count: {}", (&mut vec).len());
	// println!("{:?}", vec);

	let s:String = vec.into_iter().collect();
	let ss:Vec<String> = s.split_whitespace().map(|x| x.to_string() ).collect();
	ss
}

fn read_from_tokens(mut tokens:Vec<String>) -> Result<AST, &'static str> {
	if tokens.len() > 0 {
		let mut tokens2 = tokens.clone();
		let mut token = tokens2.remove(0);
		// println!("{:?}", token.clone());
		// println!("{:?}", tokens2.clone());
		if token == "(" {
			let mut vec:Vec<AST> = vec![];
			while tokens2[0] != ")" {
				match read_from_tokens(tokens2.clone()) {
					Ok(mut node) => vec.push(node),
					Err(e) => { return Err(e) }
				}
				tokens2.remove(0);
			}
			// tokens2.remove(0); // pop off ')'
			Ok(AST::Children(vec))
		} else if token == ")" {
			Err("unexpected )")
		} else {
			Ok(atom(&token))
		}
	} else {
		Err("unexpected EOF while reading")
	}
}

fn atom(token: &str) -> AST {
	let to_int = token.parse::<u32>();
	let to_float = token.parse::<f64>();

	if to_int.is_ok() {
		AST::Fixnum(to_int.unwrap_or_default())
	} else if to_float.is_ok() {
		AST::Float(to_float.unwrap_or_default())
	} else {
		AST::Symbol(token.to_string())
	}
}
