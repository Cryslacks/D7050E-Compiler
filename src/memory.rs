use crate::ast::*;

use std::collections::{HashMap, VecDeque};

pub type FnEnv<'a> = HashMap<&'a str, &'a Func>;

impl Func {
    pub fn get_id(&self) -> &str {
        &self.name
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    Num(i32),
    Bool(bool),
    Unit,
    Uninitialized,
    RefMut(String),
    Ref(String),
}

#[derive(Debug, Clone)]
pub enum Bc {
    Owner(String, usize),
    Ref(String, usize),
    RefMut(String, usize),
}

pub fn progam_to_env(prog: &Program) -> FnEnv {
    let program_funcs = &prog.funcs;
    program_funcs.into_iter().map(|f| (f.get_id(), f)).collect()
}

#[derive(Debug)]
pub struct VarMem(VecDeque<HashMap<String, Type>>);

impl VarMem {
    pub fn new() -> Self {
        VarMem(VecDeque::<HashMap<String, Type>>::new())
    }

    pub fn get_type(&self, id: String) -> Option<&Type> {
        self.0.iter().find_map(|hm| {
            hm.get(id.as_str())
                .map(|t| (t))
        })
    }

    pub fn get_mut_type(&mut self, id: String) -> Option<&mut Type> {
        self.0.iter_mut().find_map(|hm| hm.get_mut(id.as_str()))
    }

    pub fn new_id(&mut self, id: String, ty: Type) {
        let hm = self.0.front_mut().unwrap();
        println!("[VarMem] Inserted id {:?} into scope {:?}", &id, hm);

        hm.insert(id, ty);
    }

    pub fn update(&mut self, id: String, ty: Type) {
        println!("[VarMem] Updating environment variable {:?}", id);
        match self.get_mut_type(id.clone()) {
            Some(ot) => match ot {
                Type::Unknown => {
                    (*ot) = ty;
                }
                t => match (*t).eq(&ty) {
                    true => println!("[VarMem] Updated environment variable {:?} to type {:?}", id, ty),
                    false => panic!("[VarMem] Update types are different {:?} <> {:?}", t, ty),
                },
            },
            None => panic!("[VarMem] Error variable {:?} is not found", id),
        }
    }

    pub fn push_empty_scope(&mut self) {
        self.0.push_front(HashMap::new());
    }

    pub fn push_param_scope(&mut self, args: HashMap<String, Type>) {
        self.0.push_front(args)
    }

    pub fn pop_scope(&mut self) {
        self.0.pop_front();
    }
}

#[derive(Debug)]
pub struct Mem(VecDeque<HashMap<String, (bool, Val, usize)>>, HashMap<String, VecDeque<(String, Bc)>>);

impl Mem {
    pub fn new() -> Self {
        Mem(VecDeque::<HashMap<String, (bool, Val, usize)>>::new(), HashMap::<String, VecDeque<(String, Bc)>>::new())
    }

    pub fn get(&self, id: String) -> Option<(&Val, &usize)> {
        self.0.iter().find_map(|hm| match hm.get(id.as_str()) {
            Some((_, v, i)) => Some((v, i)),
            _ => None,
        })
    }

    pub fn get_mut(&mut self, id: String) -> Option<(&mut Val, &mut usize)> {
        self.0
            .iter_mut()
            .find_map(|hm| match hm.get_mut(id.as_str()) {
                Some((true, v, i)) => Some((v, i)),
                Some((_, v, i)) => {
                    match v {
                        Val::Uninitialized => Some((v, i)),
                        Val::RefMut(_) => {
                            println!("[Mem] Gotten {:?} in scope {:?}", v, i);
                            Some((v, i))
                        }
                        _ => panic!("[Mem] Error cannot access {:?} as a mutable", id),
                    }
                }
                _ => None,
            })
    }

    pub fn new_id(&mut self, id: String, is_mut: bool) {
        let mut scope = self.0.len();
        if scope != 0 {
            scope -= 1;
        }

        let hm = self.0.front_mut().unwrap();
        println!("[Mem] Inserted id {:?}, in scope {:?} with the id {:?}", id, hm, scope);
        hm.insert(id, (is_mut, Val::Uninitialized, scope));
    }

    pub fn add_ref(&mut self, id: String, ref_id: String, bc: Bc){
        let mut found = false;

        let hm = self.1.clone();
        let mut stack: VecDeque<(String, Bc)>;
        for item in hm.iter() {
            let mut i = 0;
            println!("[add_ref] Checking for dupes {:?} {:?}", item.0, item.1);
            stack = item.1.clone();
            for bc in stack.iter(){
                if bc.0 == ref_id.to_owned() {
                    println!("[BorrowStack] Found duplicate reference in {:?}", item.0.to_string());
                    stack.remove(i);
                    found = true;
                    break;
                }
                i += 1;
            }

            if found {
                println!("Removing: {:?}", item.0.to_owned());
                self.1.remove(item.0);
                self.1.insert(item.0.to_owned(), stack.clone());
                break;
            }
        }
        println!("[add_ref] Adding {:?} => {:?} from super {:?}", ref_id, bc.to_owned(), id);


        let mut buf_stack = match self.1.get(&id) {
                                                        Some(v) => v.clone(),
                                                        _ => VecDeque::<(String, Bc)>::new(),
                                                    };

        self.1.remove(&id);
        buf_stack.push_front((ref_id.to_owned(), bc));
        self.1.insert(id.to_owned(), buf_stack);

        println!("[BorrowStack] Added reference {:?}", self.1);
    }

    pub fn check_ref(&mut self, id: String, ref_id: String, scope: usize) {
        println!("[Borrow] Checking reference {:?}", id);
        
        let buf_stack = match self.1.get(&id) {
                                                        Some(v) => v.clone(),
                                                        _ => VecDeque::<(String, Bc)>::new(),
                                                    };
        let mut found = false;
        let mut b_scope: usize;

        for bc in buf_stack.iter(){
            b_scope = match &bc.1 {
                Bc::Ref(_, i) => *i,
                Bc::RefMut(_, i) => *i,
                Bc::Owner(_, i) => *i,
            };

            if bc.0 == ref_id.to_owned() {
                if b_scope > scope {
                    panic!("[Borrow] Error borrowed value does not live long enough")
                }else{
                    if self.get(bc.0.to_owned()) == None {
                        panic!("[Expr] Error identifier {:?} cannot be found in this scope", bc.0.to_owned());
                    }else{
                        found = true;
                        break;        
                    }
                }
            }
        }

        if !found {
            panic!("[Borrow] Error tried to access dead reference {:?}, references are concidered dead when the referenced value is changed", id);
        }
    }

    pub fn get_scope_of_id(&mut self, id: String) -> usize {

        let mut i = self.0.len();
        if i != 0 {
            i -= 1;
        }

        let vd = self.0.clone();
        for bc in vd.iter(){
            if bc.contains_key(&id.to_owned()) {
                break;
            }
            i -= 1;
        }
        i
    }

    pub fn update(&mut self, id: String, val: Val, check_refs: bool) {
        println!("[Mem] Updating memory {:?} = {:?} ({:?})", id, val, check_refs);
        
        let mut name = id.clone();
        let mut owner_id = match self.get(id.clone()){
            Some((Val::Ref(n), _)) => {
                n.clone()
            },
            Some((Val::RefMut(n), _)) => {
                n.clone()
            },
            _ => {
                match val.clone() {
                    Val::Ref(n) => {
                        n.clone()
                    },
                    Val::RefMut(n) => {
                        n.clone()
                    },
                    _ => "".to_string(),
                }
            },
        };

        if owner_id.eq(""){
            owner_id = id.to_owned();
        }

        let mut found = true;

        println!("[Mem] Updating {:?} in {:?}", id.to_owned(), owner_id);
        if check_refs { 
            let mut buf_stack = match self.1.get(&owner_id) {
                                            Some(v) => v.clone(),
                                            _ => VecDeque::<(String, Bc)>::new(),
                                        };
            println!("[BorrowStack] Checking: {:?} as {:?} for {:?}", buf_stack, id, owner_id);
            let mut i = 0;
            found = false;
            for bc in buf_stack.iter(){
                println!("{:?}", bc);
                let borr = (*bc).1.clone();
                let a = match borr {
                    Bc::Owner(name, scope) => {
                        (name, scope)
                    },
                    Bc::RefMut(name, scope) => {
                        (name, scope)
                    },
                    Bc::Ref(_, _) => {
                        panic!("[Borrow] Error cannot access reference {:?} as a mutable", id)
                    },
                };

                println!("[Borrow] Checking {:?} == {:?}", bc.0, id);
                if bc.0 == id {
                    name = a.0.to_owned();
                    found = true;
                    break;
                }

                i += 1;
            }

            if found {
                for _x in 1..=i{

                    let v = match buf_stack.pop_front() {
                                        Some((s,b)) => (s, b),
                                        _ => ("".to_string(), Bc::Owner("".to_string(), 0)),
                                    };

                    println!("[Borrow] Dropping {:?} {:?}", v.0, v.1);
                }
                println!("[BorrowStack] Removing {:?} and adding new buf_stack {:?}", owner_id.to_owned(), buf_stack);
                self.1.remove(&owner_id);
                self.1.insert(owner_id.to_owned(), buf_stack);
                println!("[BorrowStack] New stack: {:?}", self.1.clone());
            }
        }
        
        match self.get_mut(name.clone()) {
            Some(v_ref) => {
                println!("[Mem] Variable {:?} was found {:?}", name, found);

                if !found {
                    panic!("[Borrow] Error mutable borrow {:?} has been dropped due to referenced value change by owner or mutable reference", name);
                }

                *v_ref.0 = val;

                if !owner_id.eq("") && check_refs {
                    let s = self.get_scope_of_id(owner_id.to_owned());
                    self.check_ref(owner_id, id, s);
                }
            }
            None => {
                panic!("[Mem] Error variable {:?} was not found", id);
            }
        };
        println!("[BorrowStack] {:?}", self.1);
        println!("[ScopeStack]  {:?}", self.0);
    }

    pub fn push_empty_scope(&mut self) {
        self.0.push_front(HashMap::new());
    }

    pub fn push_param_scope(&mut self, args: HashMap<String, (bool, Val, usize)>) {
        self.0.push_front(args)
    }

    pub fn pop_scope(&mut self) {
        self.0.pop_front();
    }
}

