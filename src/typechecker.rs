
use crate::ast::*;
use crate::memory::*;
use std::collections::HashMap;

pub mod ast;
pub mod memory;

// For testing

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub parser, "/parser.rs");
use parser::*;

// End of testing

fn check_expr(e: &Expr, fn_env: &FnEnv, var_mem: &mut VarMem) -> Result<Type, String> {
    println!("[ExprCheck] {:?}", e);
    println!("[VarMem] {:?}", var_mem);

    match e {
        Expr::Term(l) => match l {
            Lit::Num(_) => Ok(Type::Int),
            Lit::Bool(_) => Ok(Type::Bool),
            Lit::Id(id) => match var_mem.get_type(id.to_string()) {
                Some(t) => Ok(t.clone()),
                None => Err(format!("[ExprCheck] Error variable {:?} not found (<fn_name>)", id)),
            },
            Lit::None => Ok(Type::Unknown),
        }

        Expr::Infix(l, op, r) => {
            println!("[ExprCheck] Infix");

            let lt = check_expr(l, fn_env, var_mem)?;
            let lt = match lt {
                Type::Mut(t) => *t,
                _ => lt,
            };

            let rt = check_expr(r, fn_env, var_mem)?;
            let rt = match rt {
                Type::Mut(t) => *t,
                _ => rt,
            };

            match op {
                OpCode::Add | OpCode::Mul | OpCode::Div | OpCode::Sub => {
                    if lt == Type::Int && rt == Type::Int {
                        Ok(Type::Int)
                    } else {
                        Err(format!("[ExprCheck] Error type mismatch expected Type::Int, gotten {:?}, {:?} (<fn_name>)", lt, rt))
                    }
                }
                OpCode::And | OpCode::Or => {
                    if lt == Type::Bool && rt == Type::Bool {
                        Ok(Type::Bool)
                    } else {
                        Err(format!("[ExprCheck] Error type mismatch expected Type::Bool, gotten {:?}, {:?} (<fn_name>)", lt, rt))
                    }
                }
                OpCode::Equal | OpCode::NotEqual => {
                    if lt == rt {
                        Ok(Type::Bool)
                    } else {
                        Err(format!("[ExprCheck] Error type mismatch expected Type::Bool, gotten {:?}, {:?} (<fn_name>)", lt, rt))
                    }
                }
                OpCode::Lesser | OpCode::Greater | OpCode::LesserOrEqual | OpCode::GreaterOrEqual => {
                    if lt == Type::Int && rt == Type::Int {
                        Ok(Type::Bool)
                    } else {
                        Err(format!("[ExprCheck] Error type mismatch expected Type::Int, gotten {:?}, {:?} (<fn_name>)", lt, rt))
                    }
                }
                _ => panic!("[ExprCheck] Error unsupported operation (<fn_name>)"),
            }
        }

        Expr::Prefix(op, r) => {
            println!("[ExprCheck] Prefix");
            let rt = check_expr(r, fn_env, var_mem)?;

            let ty = match op {
                OpCode::Mul | OpCode::Div | OpCode::Add | OpCode::Sub   => Type::Int,
                _                                                       => Type::Bool,
            };

            if rt == ty {
                Ok(ty)
            }else{
                Err(format!("[ExprCheck] Error type mismatch gotten {:?} (<fn_name>)", rt))
            }
        }

        Expr::Call(s, args) => {
            println!("[ExprCheck] Call");

            let args: Vec<Type> = args.clone().0.into_iter().map(|e| check_expr(&*e, fn_env, var_mem)).collect::<Result<_, _>>()?;

            println!("[ExprCheck] Calling {:?} with types {:?}", s, args);
            let f = match fn_env.get(s.as_str()) {
                Some(f) => (*f).to_owned(),
                None => Err(format!("[ExprCheck] Error function not found {:?} (<fn_name>)", s))?,
            };

            let params: Vec<Type> = (f.params.0.clone()).into_iter().map(|p| (p._type.to_owned())).collect();
            let c_params = params.to_owned();

            let arg_param: Vec<(&Type, Type)> = args.iter().zip(params).collect();

            let mut found_mismatch = false;
            for ap in arg_param {
                let arg = ap.0.to_owned();
                let par = ap.1;
                if arg != par {
                    let strip_arg = match arg {
                        Type::Mut(t) => *t,
                        _ => arg,
                    };

                    if strip_arg != par {
                        found_mismatch = true;
                        break;
                    } 
                }
            }

            if found_mismatch {
                Err(format!("[ExprCheck] Error type mismatch expected {:?} gotten {:?} (<fn_name>)", args, c_params))
            }else{
                check_fn(s, &c_params, fn_env,var_mem)
            }
        }

        Expr::Ref(e) => {
            println!("[ExprCheck] Ref");
            let ty = check_expr(e, fn_env, var_mem)?;
            match ty {
                Type::Mut(_)    => Ok(Type::Ref(Box::new(ty))),
                _               => Ok(Type::Ref(Box::new(ty))),
            }
        }

        Expr::RefMut(e) => {
            println!("[ExprCheck] RefMut");
            let t = check_expr(e, fn_env, var_mem)?;
            match t {
                Type::Mut(_)    => Ok(Type::Ref(Box::new(t))),
                _               => Err(format!("[ExprCheck] Error not mutable {:?} (<fn_name>)", e)),
            }
        }

        Expr::DeRef(e) => {
            println!("[ExprCheck] DeRef");
            let mut ty = check_expr(e, fn_env, var_mem)?;

            ty = match ty {
                Type::Mut(t) => *t,
                _ => ty,
            };

            match ty {
                Type::Ref(t) => Ok(*t),
                _ => Err(format!("[ExprCheck] Error cannot dereference {:?} of type {:?} (<fn_name>)", e, ty)),
            }
        }

        Expr::Block(b) => {
            println!("[ExprCheck] Block");
            check_stmts(b, fn_env, var_mem)
        }

        Expr::Stmt(s) => {
            println!("[ExprCheck] Stmt");
            check_stmts(&vec![s.to_owned()], fn_env, var_mem)
        },
    }
}

fn check_recursive_id<'a>(e: &Expr, fn_env: &FnEnv, var_mem: &'a mut VarMem) -> Result<&'a mut Type, String> {
    match e {
        Expr::Term(Lit::Id(id)) => match var_mem.get_mut_type(id.to_string()) {
            Some(t) => Ok(t),
            None => Err(format!("[RecursiveCheck] Error variable {:?} not found (<fn_name>)", id)),
        },

        Expr::DeRef(e) => {
            let t = check_recursive_id(e, fn_env, var_mem)?;

            let t = match t {
                Type::Mut(t) => t,
                _ => t,
            };

            match t {
                Type::Ref(t) => Ok(t),
                _ => Err(format!("[RecursiveCheck] Error cannot dereference {:?} of type {:?}", e, t)),
            }
        }
        _ => Err(format!("[RecId] Error id is not a term nor a dereference")),
    }
}

fn check_stmts(stmts: &Vec<Stmt>, fn_env: &FnEnv, var_mem: &mut VarMem) -> Result<Type, String> {

    let ss = stmts;
    var_mem.push_empty_scope();
    let mut stmt_res: Result<Type, String> = Ok(Type::Unit);
    let mut i:i32 = 0;

    for s in ss {
        i += 1;
        println!("[StmtsCheck] Stmt: {:?}", s);
        println!("[VarMem] VarMem: {:?}", var_mem);
        stmt_res = match s {
            Stmt::Let(b, id, ty, e_orig) => {
                println!("[StmtsCheck] Let");
                let t: Type = match (ty, e_orig) {
                    (Some(t), Some(e)) => { // let a: i32 = 3;
                        let e_type = check_expr(&*e, fn_env, var_mem)?;
                        let e_type = match e_type {
                            Type::Mut(t) => *t,
                            _ => e_type,
                        };

                        let var_type = t.clone();
                        let var_type = match var_type {
                            Type::Mut(t) => *t,
                            _ => var_type,
                        };

                        match var_type == e_type {
                            true => var_type,
                            false => {
                                Err(format!("[StmtsCheck] Error type mismatch, expected {:?} gotten {:?} (line {:?}, <fn_name>)", var_type, e_type, i))?
                            }
                        }
                    }
                    (None, Some(e)) => { // let a = 3;
                        let e_type = check_expr(&*e, fn_env, var_mem)?;
                        match e_type {
                            Type::Mut(t) => *t,
                            _ => e_type,
                        }
                    }
                    (Some(t), None) => t.clone(), // let a:i32;
                    _ => Type::Unknown, // let a;
                };

                let t = match b {
                    true => Type::Mut(Box::new(t)),
                    false => t,
                };

                var_mem.new_id(id.clone(), t);
                Ok(Type::Unit)
            },

            Stmt::If(expr, s, es) => {
                println!("[StmtsCheck] If");
                match check_expr(&*expr, fn_env, var_mem)? {
                    Type::Bool => {
                        let s_type = check_stmts(&s, fn_env, var_mem)?;
                        match es {
                            None => Ok(s_type),
                            Some(es) => {
                                let es_type = check_stmts(&es, fn_env, var_mem)?;

                                match s_type == es_type {
                                    true => Ok(s_type),
                                    false => {
                                        Err(format!("[StmtsCheck] Error type mismatch, arms of the if stmt does not match. Expected {:?} gotten {:?} (line {:?}, <fn_name>)",s_type, es_type, i))
                                    }
                                }
                            }
                        }
                    }
                    _ => Err(format!("[StmtsCheck] Error condition not a Bool (line {:?}, <fn_name>)", i)),
                }
            },

            Stmt::While(expr, block) => {
                println!("[StmtsCheck] While");
                match check_expr(&*expr, fn_env, var_mem) {
                    Ok(Type::Bool) => {
                        let _ = check_stmts(&block, fn_env, var_mem)?;
                        Ok(Type::Unit)
                    }
                    _ => Err(format!("[StmtsCheck] Error condition not a Bool (line {:?}, <fn_name>)", i)),                
                }
            },

            Stmt::Assign(l_expr, r_expr) => {
                println!("[StmtsCheck] Assign");
                let r_type = check_expr(&*r_expr, fn_env, var_mem)?;
                let r_type = match r_type {
                    Type::Mut(t) => *t,
                    _ => r_type,
                };

                let l_type = check_recursive_id(l_expr, fn_env, var_mem)?;

                if match l_type {
                    Type::Unknown => {
                        *l_type = r_type.clone();
                        true
                    }
                    Type::Mut(t) => match **t {
                        Type::Unknown => {
                            *t = Box::new(r_type.clone());
                            true
                        }
                        _ => **t == r_type,
                    },

                    _ => Err(format!("[StmtsCheck] Error tried to assign to non mutable (line {:?}, <fn_name>)", i))?,
                } {
                    Ok(Type::Unit)
                } else {
                    Err(format!("[StmtsCheck] Error type mismatch, cannot assign {:?} to {:?} (line {:?}, <fn_name>)", &l_type, r_type, i))
                }
            },

            Stmt::Expr(e) => {
                println!("[StmtsCheck] Expr");
                check_expr(&*e, fn_env, var_mem)
            }

            Stmt::Block(b) => {
                println!("[StmtsCheck] Block");
                check_stmts(b, fn_env, var_mem)
            }

            Stmt::Return(e) => {
                println!("[StmtsCheck] Return");
                check_expr(&*e, fn_env, var_mem)
            }

            Stmt::Semi => Ok(Type::Unit),
        };
    
        if stmt_res.is_err() {
            break;
        }
    } 

    var_mem.pop_scope();
    stmt_res
}

fn check_fn(name: &str, params: &Vec<Type>, fn_env: &FnEnv, var_mem: &mut VarMem) -> Result<Type, String> {
    if let Some(decl) = fn_env.get(name) {
        println!("[FuncCheck] Typechecking function {:?} with params {:?}", name, params);
        let id: &Vec<String> = &decl.params.0.iter().
            map(|param| (param.name.to_owned())).collect();

        let raw_params: Vec<(&String, &Type)> = id.iter().zip(params).collect();
        let vars: HashMap<String, Type> = raw_params.into_iter().map(|(s,v )| (s.clone(), v.clone())).collect();
        
        println!("[FuncCheck] Converted params into variables: {:?}", &vars);
        var_mem.push_param_scope(vars);
        let ret_type = check_stmts(&decl.body, fn_env, var_mem);
        println!("[FuncCheck] Function {:?} returned as {:?} and containing the memory \n\t{:?}", name, ret_type, var_mem);
        var_mem.pop_scope();


        if ret_type.is_err() {
            Err(ret_type.unwrap_err().replace("<fn_name>", name))
        }else{
            if ret_type.to_owned().unwrap() == decl.ret_type {
                ret_type
            } else {
                Err(format!("[FuncCheck] Error type mismatch, function {:?} returned with {:?} expected {:?}", name, ret_type.to_owned().unwrap(), decl.ret_type))
            }
        }

    } else {
        panic!("[Func] Error function named {:?} not found.");
    }
}

fn main() {
    let mut var_mem = VarMem::new();
    let program = &ProgramParser::new().parse("
    fn main() {
        let natha = 12;
    }
    
    fn add(a:i32, b:i32) -> i32 {
        a+b
    }
    ").unwrap(); 
    let fn_env = progam_to_env(program);
    let args: Vec<Type> = Vec::new();
    println!("{:?}", check_fn("main", &args, &fn_env, &mut var_mem));
}

