use std::{io::{self}};
use serde_json::Value;

pub enum Expr {
    Affect     (String   , Box<Expr>),
    And        (Box<Expr>, Box<Expr>),
    Andlasy    (Box<Expr>, Box<Expr>),
    Or         (Box<Expr>, Box<Expr>),
    Orlasy     (Box<Expr>, Box<Expr>),
    Pipeout    (Box<Expr>, Box<Expr>),
    Pipein     (Box<Expr>, Box<Expr>),
    Pipeoutsup (Box<Expr>, Box<Expr>),
    Pipeerr    (Box<Expr>, Box<Expr>),
    Chain      (Box<Expr>, Box<Expr>),
    Command    (Vec<Arg>),
}

pub enum Arg {
    Flat       (String),
    Quot       (String),
    Var        (String),
    Subsh      (Box<Expr>),
    Dquot      (Vec<Dquotword>),
}

pub enum Dquotword {
    Flat       (String),
    Var        (String),
    Subsh      (Box<Expr>),
}

fn value_to_string(valeur : &Value) -> io::Result<&str> {
    return valeur.as_str().ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("une chaine de caracteaire etais attendue")));
}

fn json_to_dquotword(json_obj : &Value) -> io::Result<Dquotword> {
    match json_obj{
        Value::Object(map) => {
            let item_type = value_to_string(map.get("type").ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("missing field: type")))?)?;
            let item_op   = value_to_string(map.get("op").ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("missing field: op")))?)?;
            let item_args = map.get("arg").ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("missing field: arg1")))?
                                      .as_array().ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("bad arg table")))?;

            match item_type {
                "Dquotword"      => {
                    match item_op{
                            "Flat"  => Ok(Dquotword::Flat(value_to_string(&item_args[0])?.to_string())),
                            "Var"   => Ok(Dquotword::Var(value_to_string(&item_args[0])?.to_string())),
                            "Subsh" => Ok(Dquotword::Subsh(Box::new(json_to_ast(&item_args[0])?))),
                            _       => Err(io::Error::new(io::ErrorKind::InvalidData, format!("problemme dans le champs op"))),
                    }
                },
                _ => Err(io::Error::new(io::ErrorKind::InvalidData, format!("valeur non attendue dans le champs type"))),
            }
        }

        _ => Err(io::Error::new(io::ErrorKind::InvalidData, format!("problemme dans la structuration du json dquotword"))),
    }
}

fn json_to_arg(json_obj : &Value) -> io::Result<Arg> {
    match json_obj{
        Value::Object(map) => {
            let item_type = value_to_string(map.get("type").ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("missing field: type")))?)?;
            let item_op   = value_to_string(map.get("op").ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("missing field: op")))?)?;
            let item_args = map.get("arg").ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("missing field: arg1")))?
                                      .as_array().ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("bad arg table")))?;

            match item_type {
                "Arg"      => {
                    match item_op{
                            "Flat"       => Ok(Arg::Flat(value_to_string(&item_args[0])?.to_string())),
                            "Quot"       => Ok(Arg::Quot(value_to_string(&item_args[0])?.to_string())),
                            "Var"        => Ok(Arg::Var(value_to_string(&item_args[0])?.to_string())),
                            "Subsh"      => Ok(Arg::Subsh(Box::new(json_to_ast(&item_args[0])?))),
                            "Dquot"      => Ok(Arg::Dquot(item_args.iter().flat_map(json_to_dquotword).collect())),
                            _            => Err(io::Error::new(io::ErrorKind::InvalidData, format!("problemme dans le champs op"))),
                    }
                },
                _ => Err(io::Error::new(io::ErrorKind::InvalidData, format!("valeur non attendue dans le champs type"))),
            }
        }

        _ => Err(io::Error::new(io::ErrorKind::InvalidData, format!("problemme dans la structuration du json arg"))),
    }
}

fn json_to_ast(json_obj : &Value) -> io::Result<Expr> {
    match json_obj{
        Value::Object(map) => {
            let item_type = value_to_string(map.get("type").ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("missing field: type")))?)?;
            let item_op   = value_to_string(map.get("op").ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("missing field: op")))?)?;
            let item_args = map.get("arg").ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("missing field: arg1")))?
                                      .as_array().ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("bad arg table")))?;

            match item_type {
                "Expr"      => {
                    match item_op{
                        "Affect"      => Ok(Expr::Affect(value_to_string(&item_args[0])?.to_string(), Box::new(json_to_ast(&item_args[1])?))),
                        "And"         => Ok(Expr::And(Box::new(json_to_ast(&item_args[0])?), Box::new(json_to_ast(&item_args[1])?))),
                        "Andlasy"     => Ok(Expr::Andlasy(Box::new(json_to_ast(&item_args[0])?), Box::new(json_to_ast(&item_args[1])?))),
                        "Or"          => Ok(Expr::Or(Box::new(json_to_ast(&item_args[0])?), Box::new(json_to_ast(&item_args[1])?))),
                        "Orlasy"      => Ok(Expr::Orlasy(Box::new(json_to_ast(&item_args[0])?), Box::new(json_to_ast(&item_args[1])?))),
                        "Pipeout"     => Ok(Expr::Pipeout(Box::new(json_to_ast(&item_args[0])?), Box::new(json_to_ast(&item_args[1])?))),
                        "Pipein"      => Ok(Expr::Pipein(Box::new(json_to_ast(&item_args[0])?), Box::new(json_to_ast(&item_args[1])?))),
                        "Pipeoutsup"  => Ok(Expr::Pipeoutsup(Box::new(json_to_ast(&item_args[0])?), Box::new(json_to_ast(&item_args[1])?))),
                        "Pipeerr"     => Ok(Expr::Pipeerr(Box::new(json_to_ast(&item_args[0])?), Box::new(json_to_ast(&item_args[1])?))),
                        "Chain"       => Ok(Expr::Chain(Box::new(json_to_ast(&item_args[0])?), Box::new(json_to_ast(&item_args[1])?))),
                        "Command"     => Ok(Expr::Command(item_args.iter().flat_map(json_to_arg).collect())),
                        _             => Err(io::Error::new(io::ErrorKind::InvalidData, format!("problemme dans le champs op"))),
                    }
                },
                _ => Err(io::Error::new(io::ErrorKind::InvalidData, format!("valeur non attendue dans le champs type"))),
            }
        }

        _ => Err(io::Error::new(io::ErrorKind::InvalidData, format!("problemme dans la structuration du json expr"))),
    }
}

pub fn load_shell_ast(input: &String) -> io::Result<Expr>{
    let data = serde_json::from_str(input)?;
    return json_to_ast(&data);
}