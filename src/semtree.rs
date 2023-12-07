use std::{collections::{HashMap}, sync::Arc};

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

#[derive(Debug, Clone)]
struct Scope<'a> {
    outer_scope: Option<&'a Scope<'a>>,
    local_variables: HashMap<Id, Vec<LocalVariable>>,
    order: Vec<LocalVariable>,
}
impl<'a> Scope<'a> {
    pub fn new() -> Self {
        Scope { outer_scope: None, local_variables: Default::default(), order: Default::default() }
    }
    pub fn new_with_outer<'b: 'a>(outer: &'b Scope<'b>) -> Self {
        Scope { outer_scope: Some(outer), local_variables: Default::default(), order: Default::default() }
    }
    fn rec_occupied(&self, lv: &LocalVariable) -> bool {
        self.order.contains(lv) || self.outer_scope.map(|x|x.rec_occupied(lv)).unwrap_or(false)
    }
    //Перекрывающиеся области видимости
    pub fn add_variable(&mut self, tree_id: &Id) -> LocalVariable {
        let candidate_name = LocalVariable(tree_id.0.clone());
        let mut testing_name = candidate_name.clone();
        let mut counter = 0;
        while self.rec_occupied(&testing_name) {
            testing_name = LocalVariable(format!("{}_{}", candidate_name.0, counter));
            counter+=1;
        }
        self.order.push(testing_name.clone());
        if let Some(m) = self.local_variables.get_mut(tree_id) {
            m.push(testing_name.clone())
        }
        else {
            self.local_variables.insert(tree_id.clone(), vec![testing_name.clone()]).unwrap();
        }
        testing_name
    }
    fn rec_get(&self, tree_id: &Id) -> Option<LocalVariable> {
        self.local_variables.get(tree_id).map(|x|x.last().unwrap().clone())
            .or(self.outer_scope.map_or(None, |x|x.rec_get(tree_id)))
    }
    pub fn get_variable(&self, tree_id: &Id) -> Option<LocalVariable> {
        self.rec_get(tree_id)
    }
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
    LocalVariable(LocalVariable),

}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LocalVariable(String);