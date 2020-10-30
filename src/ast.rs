#[derive(Debug, Clone)]
pub struct Program {
    pub funcs: Vec<Func>,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub params: Params,
    pub ret_type: Type,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Params(pub Vec<Param>);

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub mutable: bool,
    pub _type: Type,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let(bool, String, Option<Type>, Option<Box<Expr>>),
    If(Box<Expr>, Vec<Stmt>, Option<Vec<Stmt>>),
    While(Box<Expr>, Vec<Stmt>),
    Assign(Box<Expr>, Box<Expr>),
    Expr(Box<Expr>),
    Block(Vec<Stmt>),
    Return(Box<Expr>),
    Semi,
}

#[derive(Debug, Clone)]
pub struct Exprs(pub Vec<Box<Expr>>);

#[derive(Debug, Clone)]
pub enum Expr {
    Infix(Box<Expr>, OpCode, Box<Expr>),
    Prefix(OpCode, Box<Expr>),
    Call(String, Exprs),
    Term(Lit),
    Block(Vec<Stmt>),
    Ref(Box<Expr>),
    RefMut(Box<Expr>),
    DeRef(Box<Expr>),
    Stmt(Stmt),
}

#[derive(Debug, Clone)]
pub enum Lit {
    Num(i32),
    Id(String),
    Bool(bool),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Unit,
    Ref(Box<Type>),
    Mut(Box<Type>),
    Unknown,
}

#[derive(Debug, Clone)]
pub enum OpCode{
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or, 
    Not,
    Equal,
    NotEqual,
    Greater,
    Lesser,
    GreaterOrEqual,
    LesserOrEqual,
}

// println!("{}", ..)

//impl fmt::Display for Expr {
//    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//        match self {
//            Expr::LoOp(l,c,r ) => write!(f, "({} {} {})", l, c, r)?,
//            Expr::ArOp(l,c,r ) => write!(f, "({} {} {})", l, c, r)?,
//            Expr::FuncCall(s,a) => write!(f, "{} {}", s, a)?,
//            Expr::Term(s) => write!(f, "{}", s)?,
//        };
//        Ok(())
//    }
//}
