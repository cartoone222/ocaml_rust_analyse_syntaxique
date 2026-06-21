use std::collections::HashMap;
use std::env;
use std::io;
use std::process::Stdio;
use std::process::Command;
use std::io::Write;

use crate::shell_exec::ExecContext;

mod shell_rep;
mod shell_exec;

fn parse_input(input: &String) -> String {
    static PARSER: &str = "analyser_syntax_ocaml/analyser/_build/install/default/bin/analyser";

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
    let mut context = shell_exec::Context::new(HashMap::new(), 
        env::current_dir().unwrap().into_os_string().into_string().map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Le chemin courant n'est pas UTF-8"))?);

    loop {
        let mut input_buf = String::new();
        let stdin = io::stdin();
        stdin.read_line(&mut input_buf)?;

        let rep_intermediaire = parse_input(&input_buf);

        match shell_rep::load_shell_ast(&rep_intermediaire) {
            Ok(rep) => {
                        let exec_context = shell_exec::ExecContext::new(Stdio::inherit(), Stdio::inherit(), Stdio::inherit());
                        context = shell_exec::exec(&rep, context, exec_context);
                     },
            Err(e) => {println!("Error: {e}")},
        }
    }
}
