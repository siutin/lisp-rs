macro_rules! tuplet {
 { ($y:ident $(, $x:ident)*) = $v:expr } => {
    let ($y,$($x),*) = tuplet!($v ; 1 ; ($($x),*) ; ($v.get(0)) ); };
 { ($y:ident , * $x:ident) = $v:expr } => {
    let ($y, $x) = {
        if let Some((x,y)) = $v.split_first() {
            if y.is_empty() {
                (Some(x),None)
            } else {
                (Some(x),Some(y))
            }
        } else {
            (None, None)
        }
    };
};
 { $v:expr ; $j:expr ; ($y:ident $(, $x:ident)*) ; ($($a:expr),*) } => {
    tuplet!( $v ; $j+1 ; ($($x),*) ; ($($a),*,$v.get($j)) ) };
 { $v:expr ; $j:expr ; () ; $accu:expr } => { $accu }
}


fn main () {
    let v = vec![1, 2, 3, 4, 5];
    tuplet!((a,*b) = v);

    println!("a = {:?}", a);
    println!("b = {:?}", b);
    //    println!("c = {:?}", c);
    //    println!("d = {:?}", d);
}