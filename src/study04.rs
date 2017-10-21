// A Rust macro for unpacking a vector to a tuple of options
// Original from https://stackoverflow.com/questions/29504873/unpack-a-take-iterator-into-a-tuple/43967765#43967765

macro_rules! tuplet {
 { ($y:ident $(, $x:ident)*) = $v:expr } => {
    let ($y,$($x),*, _) = tuplet!($v ; 1 ; ($($x),*) ; ($v.get(0)) ); };
 { ($y:ident , * $x:ident) = $v:expr } => {
    let ($y,$x) = tuplet!($v ; 1 ; () ; ($v.get(0)) ); };
 { ($y:ident $(, $x:ident)* , * $z:ident) = $v:expr } => {
    let ($y,$($x),*, $z) = tuplet!($v ; 1 ; ($($x),*) ; ($v.get(0)) ); };
 { $v:expr ; $j:expr ; ($y:ident $(, $x:ident)*) ; ($($a:expr),*)  } => {
    tuplet!( $v ; $j+1 ; ($($x),*) ; ($($a),*,$v.get($j)) ) };
 { $v:expr ; $j:expr ; () ; ($($a:expr),*) } => {
   {
    if $v.len() >= $j {
        let remain = $v.len() - $j;
        if remain > 0 {
            ($($a),*, Some(&$v[$j..]))
        } else {
            ($($a),*, None)
        }
    } else {
        ($($a),*, None)
    }
   }
 }
}


fn main() {
    ex1();
    ex2();
    ex3();
    ex4();
}

fn ex1() {
    println!("=> ex1");

    let v = vec![1, 2, 3];

    tuplet!((a,b) = v);

    println!("a = {:?}", a);
    println!("b = {:?}", b);
}

fn ex2() {
    println!("=> ex2");

    let v = vec![1, 2, 3];

    tuplet!((a,b,c,d) = v); // return d as None

    println!("a = {:?}", a);
    println!("b = {:?}", b);
    println!("c = {:?}", c);
    println!("d = {:?}", d);
}

fn ex3() {
    println!("=> ex3");

    let v = vec![1, 2, 3, 4, 5, 6, 7, 8, 9];
    tuplet!((a,*b) = v); // rest support

    println!("a = {:?}", a);
    println!("b = {:?}", b);
}

fn ex4() {
    println!("=> ex4");

    let v = vec![1, 2, 3, 4, 5, 6, 7, 8, 9];
    tuplet!((a,b,c,*d) = v); // rest support

    println!("a = {:?}", a);
    println!("b = {:?}", b);
    println!("c = {:?}", c);
    println!("d = {:?}", d);
}