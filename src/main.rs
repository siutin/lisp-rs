use std::collections::HashMap;
use std::cell::RefCell;

#[derive(Clone, Debug)]
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

#[derive(Debug)]
enum Varible {
	Fixnum(u32),
	Float(f64),
	Symbol(String)
}

fn main() {
	// println!("Hello, world!");
	let program = "(begin (define r 10) (* pi (* r r)))";
	println!("program: {}", program);
	let tokens = tokenize(program);
	println!("tokens: {:?}", tokens);
	let ast = read_from_tokens(tokens.clone());
	println!("ast: {:?}", ast);
	if ast.is_ok() {
		let env = RefCell::new(HashMap::new());
		let p = eval(Some(ast.unwrap().result), &env);
		match p {
			Ok(r) => println!("p: {:?}", r),
			Err(e) => panic!("ERROR: {}", e)
		}
	}

}

fn tokenize(program: &str) -> Vec<String>
{
	let iterator = RefCell::new(program.chars());
	let count = iterator.borrow().clone().count();
	let mut vec:Vec<char> = Vec::with_capacity(count);

	// println!("{:?}", iterator2);

	loop {
		match iterator.borrow_mut().next() {
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

fn read_from_tokens(mut tokens:Vec<String>) -> Result<ReadFromTokenResult, &'static str> {
	if tokens.len() > 0 {
		let token = tokens.remove(0);

		if token == "(" {
			let mut vec:Vec<AST> = vec![];
			let mut tmp_tokens = tokens.clone();

			while tmp_tokens[0] != ")" {
				match read_from_tokens(tmp_tokens.clone()) {
					Ok(data) => {
						vec.push(data.result);
						tmp_tokens = data.remain.clone();
					},
					Err(e) => { return Err(e) }
				}
			}
			tmp_tokens.remove(0);
			Ok(
				ReadFromTokenResult {
					remain: tmp_tokens,
					result: AST::Children(vec)
				}
			)
		} else if token == ")" {
			Err("unexpected )")
		} else {
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

fn eval(ast_option: Option<AST>, env: &RefCell<HashMap<String, Varible>>) -> Result<Option<AST>, &'static str> {
	match ast_option {
		Some(ast) => {
			println!("{:?}", ast);
			if let AST::Children(list) = ast {
				let solved_list: Vec<Option<AST>> = {
					let has_children = list.iter().any(|x| if let AST::Children(_) = *x { true } else { false });
					if has_children {
						list.into_iter().map(|x| eval(Some(x), &env).unwrap() ).collect::<_>()
					} else {
						list.into_iter().map(|x| Some(x)).collect::<_>()
					}
				};

				if let Some(AST::Symbol(ref s0)) = solved_list[0] {
					if s0 == "define" {
						if let Some(AST::Symbol(ref s1)) = solved_list[1].clone() {
							match Some(solved_list[2].clone()) {
								Some(Some(AST::Fixnum(i))) => { env.borrow_mut().insert(s1.clone(), Varible::Fixnum(i)); },
								Some(Some(AST::Float(f))) => { env.borrow_mut().insert(s1.clone(), Varible::Float(f)); },
								Some(Some(AST::Symbol(ref s))) => {env.borrow_mut().insert(s1.clone(), Varible::Symbol(s.clone())); },
								Some(Some(AST::Children(_))) => { return Err("should not reach here"); },
								Some(None) | None => { }
							};
							return Ok(None)
						} else {
							return Err("definition name must be a symbol");
						}
					} else {
						// println!("{:?}", env.borrow());
						Ok(solved_list[0].clone())
					}
				} else {
					// println!("{:?}", env.borrow());
					Ok(solved_list[0].clone())
				}
			} else {
				// println!("{:?}", env.borrow());
				Ok(Some(ast))
			}
		},
		None => {
			Ok(None)
  		}
	}

}
