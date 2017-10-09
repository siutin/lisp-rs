fn main() {
	// println!("Hello, world!");
	let program = "(begin (define r 10) (* pi (* r r)))";
	println!("program: {}", program);
	let tokens = tokenize(program);
	println!("tokens: {:?}", tokens);
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
