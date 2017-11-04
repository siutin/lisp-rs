extern crate scheme_rs;

extern crate log;
extern crate env_logger;

use std::cell::RefCell;
use std::rc::Rc;
use scheme_rs::*;

#[test]
fn stmt1() {
    let test_result = run("(+ 1 2 3 (+ 4 5) 6)");
    assert_eq!(Ok(Some(DataType::Number(Number::Integer(21)))), test_result.value);
}

#[test]
fn stmt2() {
    let test_result = run("(- (/ (* 1 2 3 4 5) 6) 7)");
    assert_eq!(Ok(Some(DataType::Number(Number::Float(13.0)))), test_result.value);
}

#[test]
fn stmt3() {
    let test_result = run("(define r 10)(* pi (* r r))");
    assert_eq!(Ok(Some(DataType::Number(Number::Float(314.1592653589793)))), test_result.value);
}

#[test]
fn stmt4() {
    let test_result = run(r#"
                                            (begin
                                                (define circle-area (lambda (r) (* pi (* r r))))
                                                (circle-area 3) )
                                        "#);
    assert_eq!(Ok(Some(DataType::Number(Number::Float(28.274333882308138)))), test_result.value);
}

#[test]
fn stmt5() {
    let test_result = run("(if (> (* 11 11) 120) #t #f)");
    assert_eq!(Ok(Some(DataType::Bool(true))), test_result.value);
}

#[test]
fn stmt6() {
    let test_result = run(r#"
                                            (define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))
                                            (fact 10)
                                        "#);
    assert_eq!(Ok(Some(DataType::Number(Number::Integer(3628800)))), test_result.value);
}

#[test]
fn stmt7() {
    let test_result = run("(list 0 1 2 3 0 0)");
    assert_eq!(Ok(Some(DataType::List(vec![
        DataType::Number(Number::Integer(0)),
        DataType::Number(Number::Integer(1)),
        DataType::Number(Number::Integer(2)),
        DataType::Number(Number::Integer(3)),
        DataType::Number(Number::Integer(0)),
        DataType::Number(Number::Integer(0))
    ]))), test_result.value);
}

#[test]
fn stmt8() {
    let test_result = run("(car (list 0 1 2 3 0 0))");
    assert_eq!(Ok(Some(DataType::Number(Number::Integer(0)))), test_result.value);
}

#[test]
fn stmt9() {
    let test_result = run("(cdr (cdr (list 0 1 2 3 0 0)))");
    assert_eq!(Ok(Some(DataType::List(vec![
        DataType::Number(Number::Integer(2)),
        DataType::Number(Number::Integer(3)),
        DataType::Number(Number::Integer(0)),
        DataType::Number(Number::Integer(0))
    ]))), test_result.value);
}

#[test]
fn stmt10() {
    let test_result = run(r#"
                                            (define twice (lambda (x) (* 2 x)))
                                            (twice 5)
                                        "#);
    assert_eq!(Ok(Some(DataType::Number(Number::Integer(10)))), test_result.value);
}

#[test]
fn stmt11() {
    let test_result = run(r#"
                                            (define repeat (lambda (f) (lambda (x) (f (f x)))))
                                            repeat
                                        "#);
    if let Ok(Some(DataType::Lambda(_))) = test_result.value {
        assert!(true);
    } else {
        assert!(false);
    }
}

#[test]
fn stmt12() {
    let test_result = run(r#"
                                            (define twice (lambda (x) (* 2 x)))
                                            (define repeat (lambda (f) (lambda (x) (f (f x)))))
                                            ((repeat (repeat twice)) 10)
                                        "#);
    assert_eq!(Ok(Some(DataType::Number(Number::Integer(160)))), test_result.value);
}

#[test]
fn stmt13() {
    let test_result = run("(abs -42)");
    assert_eq!(Ok(Some(DataType::Number(Number::Integer(42)))), test_result.value);
}

#[test]
fn stmt14() {
    let test_result = run("(append (list 1 2 3) (list 4 5))");
    assert_eq!(Ok(Some(DataType::List(vec![
        DataType::Number(Number::Integer(1)),
        DataType::Number(Number::Integer(2)),
        DataType::Number(Number::Integer(3)),
        DataType::Number(Number::Integer(4)),
        DataType::Number(Number::Integer(5))
    ]))), test_result.value);
}

#[test]
fn stmt15() {
    {
        let test_result = run("(apply * (list 7 9))");
        assert_eq!(Ok(Some(DataType::Number(Number::Integer(63)))), test_result.value);
    }
    {
        let test_result = run("(apply (lambda (x y)(* x y)) (list 7 9))");
        assert_eq!(Ok(Some(DataType::Number(Number::Integer(63)))), test_result.value);
    }
}

#[test]
fn stmt16() {
    let test_result = run("(length (list 7 9 4 0 3))");
    assert_eq!(Ok(Some(DataType::Number(Number::Integer(5)))), test_result.value);
}

#[test]
fn stmt17() {
    {
        let test_result = run("(list? (list 7 9 4 0 3))");
        assert_eq!(Ok(Some(DataType::Bool(true))), test_result.value);
    }
    {
        let test_result = run("(list? 1)");
        assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
    }
    {
        let test_result = run("(list? 5.5)");
        assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
    }
    {
        let test_result = run("(list? \"hello\")");
        assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
    }
    {
        let test_result = run("(list? +)");
        assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
    }
    {
        let test_result = run("(symbol? (labmda (x y) (+ x y)))");
        assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
    }
}

#[test]
fn stmt18() {
    {
        let test_result = run("(number? (list 7 9 4 0 3))");
        assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
    }
    {
        let test_result = run("(number? 1)");
        assert_eq!(Ok(Some(DataType::Bool(true))), test_result.value);
    }
    {
        let test_result = run("(number? 5.5)");
        assert_eq!(Ok(Some(DataType::Bool(true))), test_result.value);
    }
    {
        let test_result = run("(number? \"hello\")");
        assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
    }
    {
        let test_result = run("(number? +)");
        assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
    }
    {
        let test_result = run("(symbol? (labmda (x y) (+ x y)))");
        assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
    }
}

fn stmt19() {
    {
        let test_result = run("(procedure? (list 7 9 4 0 3))");
        assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
    }
    {
        let test_result = run("(procedure? 1)");
        assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
    }
    {
        let test_result = run("(procedure? 5.5)");
        assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
    }
    {
        let test_result = run("(procedure? \"hello\")");
        assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
    }
    {
        let test_result = run("(procedure? +)");
        assert_eq!(Ok(Some(DataType::Bool(true))), test_result.value);
    }
    {
        let test_result = run("(procedure? (labmda (x y) (+ x y)))");
        assert_eq!(Ok(Some(DataType::Bool(true))), test_result.value);
    }
}

fn stmt20() {
    {
        let test_result = run("(symbol? (list 7 9 4 0 3))");
        assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
    }
    {
        let test_result = run("(symbol? 1)");
        assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
    }
    {
        let test_result = run("(symbol? 5.5)");
        assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
    }
    {
        let test_result = run("(symbol? \"hello\")");
        assert_eq!(Ok(Some(DataType::Bool(true))), test_result.value);
    }
    {
        let test_result = run("(symbol? +)");
        assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
    }
    {
        let test_result = run("(symbol? (labmda (x y) (+ x y)))");
        assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
    }
}

fn stmt21() {
    {
        let test_result = run("(max 7 9 4 0 3)");
        assert_eq!(Ok(Some(DataType::Number(Number::Integer(9)))), test_result.value);
    }
    {
        let test_result = run("(min 7 9 4 0 3)");
        assert_eq!(Ok(Some(DataType::Number(Number::Integer(0)))), test_result.value);
    }
}

#[derive(Debug)]
struct TestResult {
    value: Result<Option<DataType>, &'static str>,
    env: Rc<RefCell<Env>>
}

fn run(s: &str) -> TestResult {
    env_logger::init();
    let local = RefCell::new(setup());
    let env = Env {
        local,
        parent: None
    };

    let env_ref = Rc::new(RefCell::new(env));
    let result = parse(s)
        .and_then(|ast| eval(Some(ast.result), env_ref.clone()));

    TestResult {
        value: result.clone(),
        env: env_ref.clone()
    }
}