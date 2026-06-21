use std::ops::BitAnd;
use std::{collections::HashMap, path::Path};
use std::io::Stderr;
use std::process::{Command, Stdio};
use std::io::Stdin;
use std::vec;

use crate::shell_rep::{self, Arg, Dquotword};

pub struct Context {
    var: HashMap<String, String>,
    wd: String
}

pub struct ExecContext{
    inp: Stdio,
    out: Stdio,
    err: Stdio
}

impl Context {
    pub fn new(var: HashMap<String, String>, wd: String) -> Self {
        Self { var, wd}    
    }

    pub fn show(&self) {
        println!("================ variable =================");
        for (name, val) in &self.var {
            println!("{name}: \"{val}\"");
        }
        println!("================ working ==================");
        println!("{}", self.wd);

    }
}

impl ExecContext {
    pub fn new(inp : Stdio, out : Stdio, err : Stdio) -> Self {
        Self {inp, out, err}
    }
}

fn execsubsh(expr: &shell_rep::Expr) -> String {
    return "".to_string();
}

fn process_dqwords(l: &Vec<Dquotword>, con: &Context) -> String {
    let mut out: String = "".to_string();

    for i in l{
        match i {
            shell_rep::Dquotword::Flat(str)      => {out += &(" ".to_string() + str)},
            shell_rep::Dquotword::Var(v)         => {out += &(" ".to_string() + &con.var.get(v).cloned().unwrap_or("".to_string()))},
            shell_rep::Dquotword::Subsh(expr) => {out += &(" ".to_string() + &execsubsh(expr))},
        }
        
    }

    return out;
}

fn process_arg(arg: &Arg, con: &Context) -> String {
    match arg {
            shell_rep::Arg::Flat  (str)       => str.to_string(),
            shell_rep::Arg::Quot  (str)       => str.to_string(),
            shell_rep::Arg::Var   (v)         => con.var.get(v).cloned().unwrap_or("".to_string()),
            shell_rep::Arg::Subsh (expr)   => execsubsh(expr),
            shell_rep::Arg::Dquot (l) => process_dqwords(l, con),
    }
}

fn process_args(args: &Vec<shell_rep::Arg>, con: &Context) -> Vec<String> {
    let out = args.iter().clone().map(|arg| process_arg(arg, con)).collect();
    return out;
}

fn exec_commande(args: &Vec<Arg>, con: Context, excon : ExecContext) -> Context {
    let arg_str = process_args(&args, &con);

    let mut arg_except_first = arg_str.clone();
    arg_except_first.remove(0);

    match arg_str.get(0) {
        None               => Context::new(con.var, con.wd),
        Some(bin) => {
            let str_path_wd = &(con.wd.clone() + bin);
            let file_path_wd = Path::new(str_path_wd);
            let str_path_bin =&("/bin/".to_string() + bin);
            let file_path_bin = Path::new(str_path_bin);

            if file_path_wd.exists() {
                let mut child = Command::new(str_path_wd)
                                                        .stdin(excon.inp)
                                                        .stdout(excon.out)
                                                        .stderr(excon.err)
                                                        .args(arg_except_first)
                                                        .spawn();
                Context::new(con.var, con.wd)
            } else if file_path_bin.exists() {
                let mut child = Command::new(str_path_bin)
                                                        .stdin(excon.inp)
                                                        .stdout(excon.out)
                                                        .stderr(excon.err)
                                                        .args(arg_except_first)
                                                        .spawn();
                Context::new(con.var, con.wd)
            } else {
                println!("commande not found {}", bin);
                Context::new(con.var, con.wd)
            }
        }
    }
}

pub fn exec(expr: & shell_rep::Expr, con: Context, excon : ExecContext) -> Context {
    con.show();

    match expr {
        shell_rep::Expr::Command(l) => exec_commande(l, con, excon),
        _ => Context::new(con.var, con.wd),
    }
}