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
    fn eval(self) -> Result<Self, Error>;
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expression {
    And(Box<Expression>, Box<Expression>),
    Bool(bool),
    Number(u32)
}


impl Evaluter for Expression {
    fn eval(self) -> Result<Expression, Error> {
        match self {
            e@Expression::Bool(_) => Ok(e),
            e@Expression::Number(_) => Ok(e),
            Expression::And(l, r) => {
                let left = try!(l.eval());
                let right = try!(r.eval());
                match (left, right) {
                    (Expression::Bool(lb), Expression::Bool(rb)) => Ok(Expression::Bool(lb && rb)),
                    _ => Err(Error::RuntimeError("must bool".to_string()))
                }
            }
        }
    }
}

peg! grammar(r#"
use super::Expression;

#[pub]
expression -> Expression
    = and / bool / number

number -> Expression
  = [0-9]+ { Expression::Number(match_str.parse().unwrap()) }

white_space
    = " "* { () }

and -> Expression
    = l:bool white_space "&&" white_space r:expression { Expression::And(Box::new(l), Box::new(r)) }

bool -> Expression
    = literal_true / literal_false

literal_true -> Expression
  = "true" { Expression::Bool(true) }

literal_false -> Expression
  = "false" { Expression::Bool(false) }

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