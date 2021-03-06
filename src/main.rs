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
    Number(i32),
    String(String),
    UnevaluatedArray(Vec<Expression>),
    Array(Vec<Expression>),
    Nil
}

impl Evaluter for Value {
    fn eval(self) -> Result<Expression, Error> {
        use Value::*;
        match self {
            UnevaluatedArray(arr) => arr.into_iter()
                .map(|e| e.eval())
                .collect::<Result<Vec<_>, _>>()
                .map(|v| Expression::Value(Array(v))),
            val => Ok(Expression::Value(val))
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ContOperator {
    Add(Expression),
    Sub(Expression),
    Mul(Expression),
    And(Expression),
    Or(Expression),
    Link(Expression)
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Cont {
    left: Expression,
    rights: Vec<ContOperator>
}

impl Cont {
    fn transform(self) -> Expression {
        self.rights.into_iter().fold(self.left, |l, c| match c {
            ContOperator::Add(r) =>
                Expression::Operator(Operator::Add(Box::new(l), Box::new(r))),
            ContOperator::Sub(r) =>
                Expression::Operator(Operator::Sub(Box::new(l), Box::new(r))),
            ContOperator::Mul(r) =>
                Expression::Operator(Operator::Mul(Box::new(l), Box::new(r))),
            ContOperator::And(r) =>
                Expression::Operator(Operator::And(Box::new(l), Box::new(r))),
            ContOperator::Or(r) =>
                Expression::Operator(Operator::Or(Box::new(l), Box::new(r))),
            ContOperator::Link(r) =>
                Expression::Operator(Operator::Link(Box::new(l), Box::new(r)))
        })
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Operator {
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Link(Box<Expression>, Box<Expression>),
    Equal(Box<Expression>, Box<Expression>)
}

impl Evaluter for Operator {
    fn eval(self) -> Result<Expression, Error> {
        use Operator::*;
        use Expression::Value;
        use Value::*;
        match self {
            And(l, r) => {
                let left = l.eval()?;
                let right = r.eval()?;
                match (left, right) {
                    (Value(Bool(lb)), Value(Bool(rb))) => Bool(lb && rb).eval(),
                    _ => Err(Error::RuntimeError("must bool".to_string()))
                }
            },
            Or(l, r) => {
                let left = l.eval()?;
                let right = r.eval()?;
                match (left, right) {
                    (Value(Bool(lb)), Value(Bool(rb))) => Bool(lb || rb).eval(),
                    _ => Err(Error::RuntimeError("must bool".to_string()))
                }
            },
            Add(l, r) => {
                let left = l.eval()?;
                let right = r.eval()?;
                match (left, right) {
                    (Value(Number(lb)), Value(Number(rb))) => Number(lb + rb).eval(),
                    _ => Err(Error::RuntimeError("must number".to_string()))
                }
            },
            Sub(l, r) => {
                let left = l.eval()?;
                let right = r.eval()?;
                match (left, right) {
                    (Value(Number(lb)), Value(Number(rb))) => Number(lb - rb).eval(),
                    _ => Err(Error::RuntimeError("must number".to_string()))
                }
            },
            Mul(l, r) => {
                let left = l.eval()?;
                let right = r.eval()?;
                match (left, right) {
                    (Value(Number(lb)), Value(Number(rb))) => Number(lb * rb).eval(),
                    _ => Err(Error::RuntimeError("must number".to_string()))
                }
            },
            Link(l, r) => {
                let left = l.eval()?;
                let right = r.eval()?;
                match (left, right) {
                    (Value(String(lb)), Value(String(rb))) => String(
                        lb.chars().chain(rb.chars()).collect()
                    ).eval(),
                    (Value(Array(lb)), Value(Array(rb))) => Array(
                        lb.into_iter().chain(rb.into_iter()).collect()
                    ).eval(),
                    _ => Err(Error::RuntimeError("must string or array".to_string()))
                }
            }
            Equal(l, r) => {
                let left = l.eval()?;
                let right = r.eval()?;
                if left == right {
                    Bool(true).eval()
                } else {
                    Bool(false).eval()
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
    = e8

e8 -> Expression
    =l:e7 rights:e8_cont* { Cont { left: l, rights: rights }.transform() }
    / e7

e8_cont -> ContOperator
    = space "||" space r:e7 { ContOperator::Or(r) }

e7 -> Expression
    =l:e6 rights:e7_cont* { Cont { left: l, rights: rights }.transform() }
    / e6

e7_cont -> ContOperator
    = space "&&" space r:e6 { ContOperator::And(r) }

e6 -> Expression
    = o:equal { Expression::Operator(o) }
    / e5

equal -> Operator
    = l:e5 space "==" space r:e6 { Operator::Equal(Box::new(l), Box::new(r)) }

e5 -> Expression
    = l:e4 rights:e5_cont* { Cont { left: l, rights: rights }.transform() }
    / e4

e5_cont -> ContOperator
    = space "+" space r:e4 { ContOperator::Add(r) }
    / space "-" space r:e4 { ContOperator::Sub(r) }

e4 -> Expression
    = l:e3 rights:e4_cont* { Cont { left: l, rights: rights }.transform() }
    / e3

e4_cont -> ContOperator
    = space "*" space r:e3 { ContOperator::Mul(r) }

e3 -> Expression
    = l:e2 rights:e3_cont* { Cont { left: l, rights: rights }.transform() }
    / e2

e3_cont -> ContOperator
    = space "++" space r:e2 { ContOperator::Link(r) }

e2 -> Expression
    = o:(inc / dec) { Expression::Operator(o) }
    / e1

inc -> Operator
    = e:e1 "++" space !expression { Add(Box::new(e), Box::new(Expression::Value(Value::Number(1)))) }

dec -> Operator
    = e:e1 "--" space !expression { Sub(Box::new(e), Box::new(Expression::Value(Value::Number(1)))) }

e1 -> Expression
    = e0

e0 -> Expression
    = "(" space e:expression space ")" { e }
    / v:value { Expression::Value(v) }

value -> Value
    = number / boolean / string / array / nil

string -> Value
    = double_quote s:raw_str double_quote { Value::String(s) }

raw_str -> String
    = s:[^"]* { match_str.to_string() }

number -> Value
    =  "-"? [0-9]+ { Value::Number(match_str.parse().unwrap()) }

boolean -> Value
    = "true" { Value::Bool(true) }
    / "false" { Value::Bool(false) }

array -> Value
    = "[" space array:expression**(space "," space) space "]" { Value::UnevaluatedArray(array) }

nil -> Value
    = "nil" { Value::Nil }

double_quote = "\""

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