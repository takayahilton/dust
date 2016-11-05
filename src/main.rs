#![feature(plugin)]
#![plugin(peg_syntax_ext)]
#![feature(box_patterns)]

use std::io;
use std::io::prelude::*;

#[derive(Clone, PartialEq, Eq, Debug)]
enum Error {
    ParseError(grammar::ParseError),
    RuntimeError(String)
}

trait Evaluter: Sized {
    fn eval(self) -> Result<Expression, Error>;
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expression {
    Operator(Operator),
    Value(Value)
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Value {
    Bool(bool),
    Number(i32)
}

impl Evaluter for Value {
    fn eval(self) -> Result<Expression, Error> {
        Ok(Expression::Value(self))
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ContOperator {
    Add(Expression),
    Sub(Expression),
    Mul(Expression)
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Cont {
    left: Expression,
    rights: Vec<ContOperator>
}

impl Cont {
    fn transform(self) -> Expression {
        self.rights.iter().fold(self.left, |l, c| match c.clone() {
            ContOperator::Add(r) =>
                Expression::Operator(Operator::Add(Box::new(l), Box::new(r))),
            ContOperator::Sub(r) =>
                Expression::Operator(Operator::Sub(Box::new(l), Box::new(r))),
            ContOperator::Mul(r) =>
                Expression::Operator(Operator::Mul(Box::new(l), Box::new(r)))
        })
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Operator {
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>)
}

impl Evaluter for Operator {
    fn eval(self) -> Result<Expression, Error> {
        use Operator::*;
        use Expression::Value;
        use Value::*;
        match self {
            And(l, r) => {
                let left = try!(l.eval());
                let right = try!(r.eval());
                match (left, right) {
                    (Value(Bool(lb)), Value(Bool(rb))) => Bool(lb && rb).eval(),
                    _ => Err(Error::RuntimeError("must bool".to_string()))
                }
            },
            Or(l, r) => {
                let left = try!(l.eval());
                let right = try!(r.eval());
                match (left, right) {
                    (Value(Bool(lb)), Value(Bool(rb))) => Bool(lb || rb).eval(),
                    _ => Err(Error::RuntimeError("must bool".to_string()))
                }
            },
            Add(l, r) => {
                let left = try!(l.eval());
                let right = try!(r.eval());
                match (left, right) {
                    (Value(Number(lb)), Value(Number(rb))) => Number(lb + rb).eval(),
                    _ => Err(Error::RuntimeError("must number".to_string()))
                }
            },
            Sub(l, r) => {
                let left = try!(l.eval());
                let right = try!(r.eval());
                match (left, right) {
                    (Value(Number(lb)), Value(Number(rb))) => Number(lb - rb).eval(),
                    _ => Err(Error::RuntimeError("must number".to_string()))
                }
            },
            Mul(l, r) => {
                let left = try!(l.eval());
                let right = try!(r.eval());
                match (left, right) {
                    (Value(Number(lb)), Value(Number(rb))) => Number(lb * rb).eval(),
                    _ => Err(Error::RuntimeError("must number".to_string()))
                }
            }
        }
    }
}


impl Evaluter for Expression {
    fn eval(self) -> Result<Expression, Error> {
        match self {
            Expression::Value(val) => val.eval(),
            Expression::Operator(ope) => ope.eval()
        }
    }
}

peg! grammar(r#"
use super::Expression;
use super::Operator;
use super::Value;
use super::Operator::*;
use super::Cont;
use super::ContOperator;

#[pub]
expression -> Expression
    = e10

e10 -> Expression
    = l:e9 rights:e10_cont* { Cont { left: l, rights: rights }.transform() }
    / e9

e10_cont -> ContOperator
    = space "+" space r:e9 { ContOperator::Add(r) }
    / space "-" space r:e9 { ContOperator::Sub(r) }

e9 -> Expression
    = l:e8 rights:e9_cont* { Cont { left: l, rights: rights }.transform() }
    / e8

e9_cont -> ContOperator
    = space "*" space r:e8 { ContOperator::Mul(r) }

e8 -> Expression
    = e7

e7 -> Expression
    = e6

e6 -> Expression
    = e5

e5 -> Expression
    = e4

e4 -> Expression
    = e3

e3 -> Expression
    = e2

e2 -> Expression
    = e1

e1 -> Expression
    = e0

e0 -> Expression
    = "(" space e:expression space ")" { e }
    / v:value { Expression::Value(v) }

value -> Value
    = number / boolean

number -> Value
    = [0-9]+ { Value::Number(match_str.parse().unwrap()) }

boolean -> Value
    = "true" { Value::Bool(true) }
    / "false" { Value::Bool(false) }

space
    = " "* { () }
"#);

fn main() {
    print!("> ");
    io::stdout().flush().expect("Failed to flush stdout");
    let stdin = io::stdin();

    for line in stdin.lock().lines() {
        match line {
            Ok(ref line) if line.trim() == "exit" => break,
            Ok(line) => {
                println!("{:?}", grammar::expression(&line.trim())
                    .map_err(|e| Error::ParseError(e))
                    .and_then(|e| e.eval()));
                print!("> ");
                io::stdout().flush().expect("Failed to flush stdout");
            }
            Err(_) => {}
        }
    }
}

#[test]
fn test() {
    use Expression::*;
    assert_eq!(grammar::expression("true"), Ok(True));
    assert_eq!(grammar::expression("false"), Ok(False));
    assert_eq!(grammar::expression("true=false"), Ok(
        And(
            Box::new(True),
            Box::new(False)
        )));
}