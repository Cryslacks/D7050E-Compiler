use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub parser, "/parser.rs");
use parser::*;

use crate::ast::*;
use crate::memory::*;
use std::collections::HashMap;

pub mod ast;
pub mod memory;


// EVALUATING FUNCTIONS
fn eval_infix(l: Val, oc: &OpCode, r: Val) -> Val {
    match (l, oc, r) {
        (Val::Num(l), oc, Val::Num(r)) => match oc {
            OpCode::Add => Val::Num(l + r),
            OpCode::Sub => Val::Num(l - r),
            OpCode::Mul => Val::Num(l * r),
            OpCode::Div => Val::Num(l / r),
            OpCode::Equal => Val::Bool(l == r),
            OpCode::NotEqual => Val::Bool(l != r),
            OpCode::Lesser => Val::Bool(l < r),
            OpCode::LesserOrEqual => Val::Bool(l <= r),
            OpCode::Greater => Val::Bool(l > r),
            OpCode::GreaterOrEqual => Val::Bool(l >= r),
            _ => panic!("[Infix] Error evaluating num"),
        },
        (Val::Bool(l), oc, Val::Bool(r)) => Val::Bool(match oc {
            OpCode::And => l && r,
            OpCode::Or => l || r,
            OpCode::Equal => l == r,
            _ => panic!("[Infix] Error evaluating bool"),
        }),
        _ => panic!("[Infix] Error infix doesnt match anything"),
    }
}

fn eval_prefix(oc: &OpCode, r: Val) -> Val {
    match (oc, r) {
        (oc, Val::Num(r)) => match oc {
            OpCode::Sub => Val::Num(-r),
            OpCode::Add => Val::Num(r),
            _ => panic!("[Prefix] Error evaluating num"),
        },
        (oc, Val::Bool(r)) => match oc {
            OpCode::Not => Val::Bool(!r),
            _ => panic!("[Prefix] Error evaluating bool"),
        },
        _ => panic!("[Prefix] Error prefix doesnt match anything"),
    }
}

fn eval_expr(e: &Expr, m: &mut Mem, fn_env: &FnEnv) -> Val {
    
    println!("[Expr] Evaluating expr: {:?}", e);
    match e {
        Expr::Term(l) => match l {
            Lit::Num(i) => Val::Num(*i),
            Lit::Bool(b) => Val::Bool(*b),
            Lit::Id(id)  => match m.get(id.to_owned()) {
                Some((v, _)) => v.clone(),
                None => panic!("[Expr] Identifier not found {:?}", id),
            },
            _ => panic!("[Expr] Error evaluating term"),
        },
        Expr::Infix(l, op, r) => {
            let l = eval_expr(l, m, fn_env);
            let r = eval_expr(r, m, fn_env);
            eval_infix(l, op, r)
        }
        Expr::Prefix(op, r) => {
            let r = eval_expr(r, m, fn_env);
            eval_prefix(op, r)
        },
        Expr::Ref(e) => match &**e {
            Expr::Term(l) => match l {
                    Lit::Id(id) => match m.get(id.to_owned()){
                        Some(_) => Val::Ref(id.to_owned()),
                        None => panic!("[Expr] Error identifier not found {:?}", id),
                    },
                    _ => panic!("[Expr] Error Ref on non identifier"),
            },
            _ => panic!("[Expr] Error Ref on non term"),
        },
        Expr::RefMut(e) => match &**e {
            Expr::Term(l) => match l {
                    Lit::Id(id) => match m.get(id.to_owned()){
                        Some(_) => Val::RefMut(id.to_owned()),
                        None => panic!("[Expr] Error identifier not found {:?}", id),
                    },
                    _ => panic!("[Expr] Error RefMut on non identifier"),
            },
            _ => panic!("[Expr] Error RefMut on non term"),
        },
        Expr::DeRef(e) => {
            let ev = eval_expr(e, m, fn_env);
            println!("[Expr] Dereferencing {:?}", ev);

            match ev {
                Val::Ref(id) => eval_expr(&Expr::Term(Lit::Id(id)), m, fn_env),
                Val::RefMut(id) => eval_expr(&Expr::Term(Lit::Id(id)), m, fn_env),
                _ => panic!("[Expr] Error dereference failed"),
            }
        },

        Expr::Stmt(s) => {
            println!("[Expr] Interpreting stmt: {:?}", s);
            eval_stmts(&vec![s.to_owned()], m, fn_env)
        },

        Expr::Call(name, exprs) => {
            println!("[Expr] Calling function named {:?} with arguments {:?}", name, exprs);
            
            let expr_list: Vec<Box<Expr>> = exprs.0.clone();
            let mut mut_refs: Vec<String> = Vec::new();

            for e in expr_list.iter(){
                match &**e {
                    Expr::RefMut(s) => {
                        let id = deref_reference(s).to_owned();
                        if mut_refs.contains(&id){
                            panic!("[BorrowCheck] Error cannot borrow mutably {:?} more than once", id);
                        }
                        mut_refs.push(id)
                    },
                    Expr::Ref(s) => {
                        let id = deref_reference(s).to_owned();
                        if mut_refs.contains(&id){
                            panic!("[BorrowCheck] Error cannot borrow {:?} because its already borrowed as mutable", id);
                        }
                    },
                    _ => {}
                };

                println!("{:?}", **e);
            }


            let args: Vec<Val> = exprs.0.iter().map(|expr| eval_expr(expr, m, fn_env)).collect();

            eval_fn(name, &args, m, fn_env)

        }

        _ => unimplemented!("[Expr] Unimplemented expr"),
    }
}

fn deref_reference(e: &Expr) -> String {
    match e {
        Expr::Term(Lit::Id(id)) => id.to_owned(),
        Expr::DeRef(e) => {
            deref_reference(e)
        },
        _ => panic!("[Deref] Error no dereference passed as arg")
    }
}

fn eval_stmts(stmts: &Vec<Stmt>, m: &mut Mem, fn_env: &FnEnv) -> Val {
    let ss = stmts;

    m.push_empty_scope();

    let mut stmt_val = Val::Unit;

    for s in ss {
        println!("[Stmt] Stmt: {:?}", s);
        println!("[Stmt] Mem: {:?}", m);
        stmt_val = match s {
            Stmt::Let(b, id, _, e_orig) => {
                println!("[Stmt] Let");
                m.new_id(id.to_owned(), *b);
                match e_orig {
                    Some(e) => {
                        let r_expr = &(e.clone());
                        let mut assign_id = "".to_string();
                        let mut assign_scope = 0;
                        let r = match &**r_expr {
                            Expr::DeRef(e) => {
                                let r_ref = deref_reference(e);
                                assign_id = r_ref;
                                assign_scope = m.get_scope_of_id(id.to_owned());
                                eval_expr(r_expr, m, fn_env)
                            }
                            _ => {
                                eval_expr(r_expr, m, fn_env)
                            }
                        };
                        let val = r.clone();
                        let scope = m.get_scope_of_id(id.to_owned());
                        match r {
                            Val::Ref(s) => {
                                m.add_ref(s.to_owned(),id.to_owned(), Bc::Ref(s.to_owned(), scope))
                            },
                            Val::RefMut(s) => {
                                m.add_ref(s.to_owned(),id.to_owned(), Bc::RefMut(s.to_owned(), scope))
                            },
                            _ => {
                                m.add_ref(id.to_owned(), id.to_owned(), Bc::Owner(id.to_owned(), scope))
                            },
                        };

                        m.update(id.to_owned(), val, false);
                        Val::Unit
                    },
                    _ => Val::Unit,
                }

            },
            Stmt::Assign(l_expr, r_expr) => {
                println!("[Stmt] Assign");
                let id = deref_reference(l_expr);
                let v: Val;
                match &**r_expr {
                    Expr::DeRef(e) => {
                        let r_ref = deref_reference(e);
                        let i = m.get_scope_of_id(id.to_owned());
                        v = eval_expr(r_expr, m, fn_env);
                        println!("[Deref] {:?} {:?} {:?}", id.to_owned(), r_ref, i);
                        m.update(id.to_owned(), v, true)
                    },
                    Expr::Ref(e) => {
                        let r_ref = deref_reference(e);
                        let i = m.get_scope_of_id(r_ref.to_owned());
                        m.add_ref(r_ref.to_owned(), id.to_owned(), Bc::Ref(r_ref.to_owned(), i));
                        v = eval_expr(r_expr, m, fn_env);
                        m.update(id.to_owned(), v, false)
                    },
                    Expr::RefMut(e) => {
                        let r_ref = deref_reference(e);
                        let i = m.get_scope_of_id(r_ref.to_owned());
                        m.add_ref(r_ref.to_owned(),id.to_owned(), Bc::RefMut(r_ref.to_owned(), i));
                        v = eval_expr(r_expr, m, fn_env);
                        m.update(id.to_owned(), v, false)
                    }
                    _ => {
                        v = eval_expr(r_expr, m, fn_env);
                        println!("[Deref] {:?} ", id.to_owned());
                        m.update(id.to_owned(), v, true)
                    }
                };
                Val::Unit
            },
            Stmt::If(expr,s,es) => {
                println!("[Stmt] If");
                
                match (eval_expr(expr, m, fn_env), es) {
                    (Val::Bool(true), _) => eval_stmts(s, m, fn_env),
                    (Val::Bool(false), Some(es)) => eval_stmts(es, m, fn_env),
                    (Val::Bool(_), _) => Val::Uninitialized,
                    _ => panic!("[Stmt] Error if resulted in non boolean condition"),
                }
            },
            Stmt::While(expr,block) => {
                println!("[Stmt] While");
                while match eval_expr(expr, m, fn_env)  {
                    Val::Bool(b) => b,
                    _ => panic!("[Stmt] Error while resulted in non boolean condition"),
                } {
                    eval_stmts(block, m, fn_env);
                }

                Val::Unit
            },

            Stmt::Expr(e) => {
                println!("[Stmt] Expr");
                eval_expr(e, m, fn_env)
            },

            Stmt::Block(block) => {
                println!("[Stmt] Block");
                eval_stmts(block, m, fn_env)
            },

            Stmt::Return(e) => {
                let ret_val = eval_expr(e, m, fn_env);
                println!("[Stmt] Returned with value: {:?}", ret_val);
                ret_val
            } 

            Stmt::Semi => Val::Unit,
            
        }
    }

    m.pop_scope();
    stmt_val
}

pub fn eval_fn(name: &str, params: &Vec<Val>, m: &mut Mem, fn_env: &FnEnv) -> Val {
    if let Some(decl) = fn_env.get(name) {
        println!("[Func] Evaluating function {:?} with params {:?}", name, params);
        let id: &Vec<(bool, String)> = &decl.params.0.iter().
            map(|param| (param.mutable, param.name.to_owned())).collect();

        let raw_params: Vec<(&(bool, String), &Val)> = id.iter().zip(params).collect();
        let vars: HashMap<String, (bool, Val, usize)> = raw_params.into_iter().map(|(s,v )| (s.1.clone(), (s.0, v.clone(), 0))).collect();
        
        println!("[Func] Converted params into variables: {:?}", &vars);
        m.push_param_scope(vars);
        let ret_val = eval_stmts(&decl.body, m, fn_env);
        println!("[Func] Function {:?} returned as {:?} and containing the memory \n\t{:?}", name, ret_val, m);
        m.pop_scope();
        ret_val

    } else {
        panic!("[Func] Error function named {:?} not found.");
    }

}
// END OF EVALUATING FUNCTIONS

fn main() {
    let mut m = Mem::new();
    let program = &ProgramParser::new().parse(r#"
    fn main() {
        let mut a = 3;
        let b = &mut a;
    
        while *b <= 3 {
            *b = add(*b, 1);
        }
    
        let d = *b / 3;
    }
    
    fn add(a:i32, b:i32) -> i32 {
        a+b
    }
    
    "#).unwrap();
    let fn_env = progam_to_env(program);
    let args: Vec<Val> = Vec::new();
    println!("{:?}", eval_fn("main", &args, &mut m, &fn_env));
}

#[test]
fn borrow_test_scope(){
    let mut m = Mem::new();
    let program = &ProgramParser::new().parse(r#"
        fn main() {
            let borrowed;
            if true {
                let if_scope = 1;
                borrowed = &if_scope;
            }
            let another_scope = *borrowed; // <-- Error the owners scope is gone
        }
    "#).unwrap();
    let fn_env = progam_to_env(program);
    let args: Vec<Val> = Vec::new();
    println!("{:?}", eval_fn("main", &args, &mut m, &fn_env));
}

#[test]
fn borrow_test_refchanged(){
    let mut m = Mem::new();
    let program = &ProgramParser::new().parse(r#"
        fn main() {
            let mut a = 1;
            let mut b = 2;
            let mut c = &mut a;
            c = &mut b; // first reference to a is dropped in the stack since we changed what 'borrowed' reference to
        }
    "#).unwrap();
    let fn_env = progam_to_env(program);
    let args: Vec<Val> = Vec::new();
    println!("{:?}", eval_fn("main", &args, &mut m, &fn_env));
}

#[test]
fn borrow_test_mut(){
    let mut m = Mem::new();
    let program = &ProgramParser::new().parse(r#"
    fn main() {
        let mut a = 0;
        let mut b = 0;
        let b1 = &mut b;
        let a1 = &mut a;
        *b1 = *b1 + 1;
        *a1 = *a1 + 1;
    } 
    "#).unwrap();
    let fn_env = progam_to_env(program);
    let args: Vec<Val> = Vec::new();
    println!("{:?}", eval_fn("main", &args, &mut m, &fn_env));
}

#[test]
fn borrow_test_func(){
    let mut m = Mem::new();
    let program = &ProgramParser::new().parse(r#"
    fn f(i:&mut i32, j:&mut i32) -> i32 {
        *i

    }

    fn main() {
        let mut a = 0;
        let x = f(&mut a, &mut a);
        
    }
    "#).unwrap();
    let fn_env = progam_to_env(program);
    let args: Vec<Val> = Vec::new();
    println!("{:?}", eval_fn("main", &args, &mut m, &fn_env));
}