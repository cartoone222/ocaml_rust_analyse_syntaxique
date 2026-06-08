use std::{io::{self}};
use std::process::{Command, Stdio};
use std::io::Write;
use serde_json::Value;

enum Expr {
    Number(i64),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Null,
}

fn eval(expr: &Expr) -> i64 {
    match expr {
        Expr::Number(n) => *n,
        Expr::Add(a, b) => eval(a) + eval(b),
        Expr::Sub(a, b) => eval(a) - eval(b),
        Expr::Mul(a, b) => eval(a) * eval(b),
        Expr::Div(a, b) => eval(a) / eval(b),
        Expr::Null => 0,
    }
}

fn value_to_string(valeur : &Value) -> io::Result<&str> {
    return valeur.as_str().ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("op must be string")));
}

fn parse_json_data(json_obj : &Value) -> io::Result<Expr> {
    match json_obj{
        Value::Object(map) => {
            let op = value_to_string(map.get("op").ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("missing field: arg1")))?)?;

            let arg = map.get("arg").ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("missing field: arg1")))?
                                      .as_array().ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("bad arg table")))?;

            match op{
                "add" => Ok(Expr::Add(Box::new(parse_json_data(&arg[0])?), Box::new(parse_json_data(&arg[1])?))),
                "sub" => Ok(Expr::Sub(Box::new(parse_json_data(&arg[0])?), Box::new(parse_json_data(&arg[1])?))),
                "mul" => Ok(Expr::Mul(Box::new(parse_json_data(&arg[0])?), Box::new(parse_json_data(&arg[1])?))),
                "div" => Ok(Expr::Div(Box::new(parse_json_data(&arg[0])?), Box::new(parse_json_data(&arg[1])?))),
                _     => Err((|| io::Error::new(io::ErrorKind::InvalidData, format!("bad op")))()),
            }
        }

        Value::Number(i) =>{
            let out = i.as_i64().ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("bad number")))?;
            Ok(Expr::Number(out))
        }

        _ => {Ok(Expr::Null)}
    }
}

fn parse_json_string(input: &String) -> io::Result<Expr> {
    let data = serde_json::from_str(input)?;
    return parse_json_data(&data);
}

fn parse_input(input: &String) -> String {
    static PARSER: &str = "analyser_syntax_ocamel/analyser/_build/install/default/bin/analyser";

    let mut parser_proces = Command::new(PARSER)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("parser problemme");

    let pin = parser_proces.stdin.as_mut().unwrap();
    pin.write_all(input.as_bytes()).unwrap();

    let pout = parser_proces.wait_with_output().unwrap();
    let binding = String::from_utf8_lossy(&pout.stdout);
    let output = binding.as_ref();

    println!("Output: {}", output);
    return output.to_string();
}

fn main() -> io::Result<()> {
    loop{
        let mut input_buf = String::new();
        let stdin = io::stdin();
        stdin.read_line(&mut input_buf)?;

        let rep_intermediaire = parse_input(&input_buf);

        match parse_json_string(&rep_intermediaire) {
            Ok(expr) => println!("{}", eval(&expr)),
            Err(e) => println!("Error: {e}"),
        }
    }

    // Ok(())
}