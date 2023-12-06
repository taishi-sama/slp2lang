use std::collections::{HashMap};

use crate::{symbols::{Id, RawSymbols}, types::SLPType, ast::{Loc, ProgramFile, FunctionBody, ExternFunctionBody}, errors::SemTreeBuildErrors};
#[derive(Debug, Clone)]
pub struct SemanticTree{ 
    //Replace when supporting compilation of many files
    symbols: RawSymbols,
    root: ProgramRoot,
    //Переменные пересекающихся областей определения переименовываются.
    names: HashMap<Id, String>
}
impl SemanticTree {
    pub fn new(pf: &ProgramFile, sy: &RawSymbols) -> Result<Self, Vec<SemTreeBuildErrors>> {
        let mut st = SemanticTree{root: ProgramRoot { funcs: vec![], extern_funcs: vec![] }, names: HashMap::new(), symbols: sy.clone()};
        st.visit_program_file(pf)?;
        
        Ok(st)
    }
    fn visit_program_file(&mut self, pf: &ProgramFile) -> Result<(), Vec<SemTreeBuildErrors>>{
        let mut functions = vec![];
        let mut extern_functions = vec![];
        let mut errors = vec![];
        for t in &pf.declarations {
            match t {
                crate::ast::Declaration::Function(x) => match self.visit_function_decl(x) {
                    Ok(f) => functions.push(f),
                    Err(e) => errors.push(e),
                },
                crate::ast::Declaration::ExternFunction(x) => match self.visit_extern_function_decl(x) {
                    Ok(f) => functions.push(f),
                    Err(e) => errors.push(e),
                },
                crate::ast::Declaration::TypeDeclSection(x) => todo!(),
            }
        }
        if errors.is_empty() {
            self.root = ProgramRoot{funcs: functions, extern_funcs: extern_functions};
            Ok(())
        } else {Err(errors)}
    }
    fn visit_function_decl(&mut self, func: &FunctionBody) -> Result<Function, SemTreeBuildErrors> {
        todo!()
    }
    fn visit_extern_function_decl(&mut self, func: &ExternFunctionBody) -> Result<Function, SemTreeBuildErrors> {
        todo!()
    }
}

struct Scope {
    
}

#[derive(Debug, Clone)]
pub struct ProgramRoot {
    pub funcs: Vec<Function>,
    pub extern_funcs: Vec<ExternFunction>
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
pub struct ExternFunction {
    pub function_name:Id,
    pub function_args:Vec<(Id, SLPType)>,
    pub return_arg: SLPType,
    //pub body: Vec<Statement>,
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
    LocalVariable(String),

}

