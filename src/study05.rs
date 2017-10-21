fn main() {
    let program = "(begin (define r 10) (* pi (* r r )))";
    original(program);
    modified(program);
}

fn original(program: &str) {
    let mut iterator = Box::new(program.chars());
    let count = iterator.clone().count();
    let mut vec: Vec<char> = Vec::with_capacity(count);

    loop {
        match iterator.next() {
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
            }
            None => { break; }
        }
    }

    println!("{:?}", vec);
}

fn modified(program: &str) {
    let mut iterator = Box::new(program.chars());
    let count = iterator.clone().count();

//    let vec = iterator.fold(Vec::with_capacity(count), |mut acc, x| {
//        if x == '(' {
//            acc.extend(vec![' ', '(', ' '])
//        } else if x == ')' {
//            acc.extend(vec![' ', ')', ' '])
//        } else {
//            acc.push(x)
//        }
//        acc
//    });

//    let vec = iterator.fold(Vec::with_capacity(count), |mut acc, x| {
//        match x {
//            '(' | '(' => acc.extend(vec![' ', x, ' ']),
//            _ => acc.push(x)
//        }
//        acc
//    });

    let vec = iterator.fold(Vec::with_capacity(count), |mut acc, x| {
        if x == '(' || x == ')' {
            acc.extend(vec![' ', x, ' '])
        } else {
            acc.push(x)
        }
        acc
    });

    println!("{:?}", vec);
}
