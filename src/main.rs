#[derive(Debug)]
enum AST {
	Fixnum(u32),
	Float(f64),
	Symbol(String),
	Children(Vec<AST>)
}

#[derive(Debug)]
struct ReadFromTokenResult {
	remain: Vec<String>,
	result: AST
}

fn main() {
	// println!("Hello, world!");
	let program = "(begin (define r 10) (* pi (* r r)))";
	println!("program: {}", program);
	let tokens = tokenize(program);
	println!("tokens: {:?}", tokens);
	let ast = read_from_tokens(tokens.clone(), 0);
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

fn read_from_tokens(mut tokens:Vec<String>, depth: i32) -> Result<ReadFromTokenResult, &'static str> {
	if tokens.len() > 0 {
		let mut token = tokens.remove(0);

		// println!("");
		// println!("tokens={:?}", tokens);
		// println!("token={:?}", token.clone());

		if token == "(" {
			let mut vec:Vec<AST> = vec![];
			let mut tmp_tokens = tokens.clone();

			println!("{} before tmp_tokens: {:?}", depth, tmp_tokens);

			while tmp_tokens[0] != ")" {
				match read_from_tokens(tmp_tokens.clone(), depth + 1) {
					Ok(mut data) => {
						vec.push(data.result);
						tmp_tokens = data.remain.clone();
						println!("{} nested {:?}", depth, data.remain);
					},
					Err(e) => { return Err(e) }
				}
			}
			tmp_tokens.remove(0);
			println!("{} after tmp_tokens: {:?}", depth, tmp_tokens);
			Ok(
				ReadFromTokenResult {
					remain: tmp_tokens,
					result: AST::Children(vec)
				}
			)
		} else if token == ")" {
			Err("unexpected )")
		} else {
			println!("{} symbol", depth);
			Ok(
				ReadFromTokenResult {
					remain: tokens,
					result: atom(&token)
				}
			)
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

// fn eval(ast: AST) -> Result<AST, &'static str> {
// 	match ast {
// 	AST::Symbol(s) => {
// 		Ok()
// 	},
// 	AST::Fixnum(n) => {},
// 	AST::Float(f) => {},
// 	AST::Children(l) => {
//
// 	}
// }
