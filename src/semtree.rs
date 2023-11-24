use std::collections::{HashMap};

use crate::{symbols::{Id, RawSymbols}, types::SLPType, ast::{Loc, ProgramFile}, errors::SemTreeBuildErrors};
#[derive(Debug, Clone)]
pub struct SemanticTree{ 
    root: ProgramRoot,
    //Переменные пересекающихся областей определения переименовываются.
    names: HashMap<Id, String>
}
impl SemanticTree {
    pub fn new(pf: &ProgramFile, sy: &RawSymbols) -> Result<Self, Vec<SemTreeBuildErrors>> {
        let mut st = SemanticTree{root: ProgramRoot { funcs: vec![] }, names: HashMap::new()};
        st.visit_program_file(pf)?;
        
        Ok(st)
    }
    fn visit_program_file(&mut self, pf: &ProgramFile) -> Result<(), Vec<SemTreeBuildErrors>>{
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct ProgramRoot {
    pub funcs: Vec<Function>
}
#[derive(Debug, Clone)]
pub struct Function {
    pub function_name:Id,
    pub function_args:Vec<(Id, SLPType)>,
    pub return_arg: SLPType,
    pub body: Vec<Statement>,
    pub loc: Loc,
}
#[derive(Debug, Clone)]
pub enum Statement{
    CodeBlock(Loc, Vec<Statement>),
    Print(Loc, Box<Expr>),
    FunctionCall(Loc, FunctionCall),
    //RHS, LHS
    Assignment(Loc, Box<Expr>, Box<Expr>),
    If(Loc, Box<Expr>, Box<Statement>, Option<Box<Statement>>),
    While(Loc, Box<Expr>, Box<Statement>),
    RepeatUntil(Loc, Box<Expr>, Box<Statement>),
    //Expand single declaration in multiple varDecl
    VarDecl(Loc, VarDecl),
    Empty()
}
#[derive(Debug, Clone)] 
pub struct VarDecl{
    pub id: Id,
    pub ty: SLPType,
    pub init_expr: Option<Expr>,
}
#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub func: Box<Expr>, 
    pub args: Vec<Expr>,
    pub ret_type: SLPType,
}
#[derive(Debug, Clone)]
pub struct Expr {
    pub ret_type: SLPType, 
    pub loc: Loc,
    pub kind: ExprKind
}
#[derive(Debug, Clone)]
pub enum ExprKind {
    
}

