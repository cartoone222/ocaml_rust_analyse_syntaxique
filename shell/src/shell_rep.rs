
enum Expr {
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

enum Arg {
    Flat       (String),
    Quot       (String),
    Var        (String),
    Subsh      (Box<Expr>),
    Dquot      (Vec<Dquotword>),
}

enum Dquotword {
    Flat       (String),
    Var        (String),
    Subsh      (Box<Expr>),
}

fn loadShellAST() {
    
}