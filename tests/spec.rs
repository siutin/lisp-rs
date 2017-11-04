extern crate scheme_rs;

extern crate log;
extern crate env_logger;

use std::cell::RefCell;
use std::rc::Rc;
use scheme_rs::*;

#[test]
fn if_expression_test() {
    let test_result = run("(if (> (* 11 11) 120) #t #f)");
    assert_eq!(Ok(Some(DataType::Bool(true))), test_result.value);
}

#[test]
fn variable_retrieving_test() {
    let test_result = run("(define r 10)(* pi (* r r))");
    assert_eq!(Ok(Some(DataType::Number(Number::Float(314.1592653589793)))), test_result.value);
}

#[test]
fn lambda_retrieving_test() {
    let test_result = run(r#"
    (define circle-area (lambda (r) (* pi (* r r))))
    (circle-area 3)
    "#);
    assert_eq!(Ok(Some(DataType::Number(Number::Float(28.274333882308138)))), test_result.value);
}

#[test]
fn recursive_lambda_test() {
    let test_result = run(r#"
    (define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))
    (fact 10)
    "#);
    assert_eq!(Ok(Some(DataType::Number(Number::Integer(3628800)))), test_result.value);
}

#[test]
fn lambda_call_test() {
    let test_result = run(r#"
    (define twice (lambda (x) (* 2 x)))
    (twice 5)
    "#);
    assert_eq!(Ok(Some(DataType::Number(Number::Integer(10)))), test_result.value);
}

#[test]
fn nested_lambda_test() {
    {
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
    {
        let test_result = run(r#"
        (define add3
            (lambda (x y z)
                (+ ((lambda (x y)
                     (+ x y)) x y) z)))
        (add3 2 3 4)
        "#);
        assert_eq!(Ok(Some(DataType::Number(Number::Integer(9)))), test_result.value);
    }
}

#[test]
fn complex_lambda_test() {
    let test_result = run(r#"
    (define twice (lambda (x) (* 2 x)))
    (define repeat (lambda (f) (lambda (x) (f (f x)))))
    ((repeat (repeat twice)) 10)
    "#);
    assert_eq!(Ok(Some(DataType::Number(Number::Integer(160)))), test_result.value);
}

#[test]
fn state_test() {
    let env_ref = default_env();
    let test_result0 = run_with_env("s", env_ref.clone());
    assert_eq!(Err("symbol is not defined."), test_result0.value);

    let test_result1 = run_with_env("(define s \"hello world\")", env_ref.clone());
    assert_eq!(Ok(None), test_result1.value);

    let test_result2 = run_with_env("s", env_ref.clone());
    assert_eq!(Ok(Some(DataType::Symbol("hello world".to_string()))), test_result2.value);
}

mod op {
    use super::*;

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
}

mod std_function {
    use super::*;

    #[test]
    fn list() {
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
    fn car() {
        let test_result = run("(car (list 0 1 2 3 0 0))");
        assert_eq!(Ok(Some(DataType::Number(Number::Integer(0)))), test_result.value);
    }

    #[test]
    fn cdr() {
        let test_result = run("(cdr (cdr (list 0 1 2 3 0 0)))");
        assert_eq!(Ok(Some(DataType::List(vec![
            DataType::Number(Number::Integer(2)),
            DataType::Number(Number::Integer(3)),
            DataType::Number(Number::Integer(0)),
            DataType::Number(Number::Integer(0))
        ]))), test_result.value);
    }

    #[test]
    fn abs() {
        let test_result = run("(abs -42)");
        assert_eq!(Ok(Some(DataType::Number(Number::Integer(42)))), test_result.value);
    }

    #[test]
    fn append() {
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
    fn apply() {
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
    fn length() {
        let test_result = run("(length (list 7 9 4 0 3))");
        assert_eq!(Ok(Some(DataType::Number(Number::Integer(5)))), test_result.value);
    }

    #[test]
    fn max_min() {
        {
            let test_result = run("(max 7 9 4 0 3)");
            assert_eq!(Ok(Some(DataType::Number(Number::Integer(9)))), test_result.value);
        }
        {
            let test_result = run("(min 7 9 4 0 3)");
            assert_eq!(Ok(Some(DataType::Number(Number::Integer(0)))), test_result.value);
        }
    }

    #[test]
    fn not() {
        {
            let test_result = run("(not #t)");
            assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
        }
        {
            let test_result = run("(not #f)");
            assert_eq!(Ok(Some(DataType::Bool(true))), test_result.value);
        }
        {
            let test_result = run("(not 1)");
            assert_eq!(Err("not function requires an argument of type 'boolean'"), test_result.value);
        }
    }

    mod type_checking_function {
        use super::*;

        #[test]
        fn list_q() {
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
                let test_result = run("(list? (lambda (x y) (+ x y)))");
                assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
            }
        }

        #[test]
        fn number_q() {
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
                let test_result = run("(number? (lambda (x y) (+ x y)))");
                assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
            }
        }

        #[test]
        fn procedure_q() {
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
                let test_result = run("(procedure? (lambda (x y) (+ x y)))");
                assert_eq!(Ok(Some(DataType::Bool(true))), test_result.value);
            }
        }

        #[test]
        fn symbol_q() {
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
                let test_result = run("(symbol? (lambda (x y) (+ x y)))");
                assert_eq!(Ok(Some(DataType::Bool(false))), test_result.value);
            }
        }
    }
}


#[derive(Debug)]
struct TestResult {
    value: Result<Option<DataType>, &'static str>,
    env: Rc<RefCell<Env>>
}

fn default_env() -> Rc<RefCell<Env>> {
    let local = RefCell::new(setup());
    let env = Env {
        local,
        parent: None
    };

    let env_ref = Rc::new(RefCell::new(env));
    env_ref
}

fn run(s: &str) -> TestResult {
    env_logger::init();
    run_with_env(s, default_env().clone())
}

fn run_with_env(s: &str, env_ref: Rc<RefCell<Env>>) -> TestResult {
    env_logger::init();
    let result = parse(s)
        .and_then(|ast| eval(Some(ast.result), env_ref.clone()));

    TestResult {
        value: result.clone(),
        env: env_ref.clone()
    }
}