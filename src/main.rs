#![feature(plugin)]
#![plugin(peg_syntax_ext)]

use std::io;
use std::io::prelude::*;



#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expression {
    Equal(Box<Expression>, Box<Expression>),
    True,
    False
}

peg! grammar(r#"
use super::Expression;

#[pub]
expression -> Expression
    = equal / bool

white_space
    = " "* { () }

equal -> Expression
    = l:bool white_space "==" white_space r:expression { Expression::Equal(Box::new(l), Box::new(r)) }

bool -> Expression
    = literal_true / literal_false

literal_true -> Expression
  = "true" { Expression::True }

literal_false -> Expression
  = "false" { Expression::False }

"#);

fn main() {
    print!("> ");
    io::stdout().flush().expect("Failed to flush stdout");
    let stdin = io::stdin();

    for line in stdin.lock().lines() {
        match line {
            Ok(ref line) if line == "exit" => break,
            Ok(line) => {
                println!("{:?}", grammar::expression(&line));
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
        Equal(
            Box::new(True),
            Box::new(False)
        )));
}