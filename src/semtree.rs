use std::{collections::HashMap, sync::Arc};

use crate::{
    ast::{self, Expr, ExternFunctionBody, FunctionBody, Loc, ProgramFile, Statement},
    errors::SemTreeBuildErrors,
    symbols::{Id, RawSymbols},
    types::SLPType,
};
#[derive(Debug, Clone)]
pub struct SemanticTree {
    //Replace when supporting compilation of many files
    symbols: RawSymbols,
    pub root: ProgramRoot,
    //Переменные пересекающихся областей определения переименовываются.
    names: HashMap<Id, String>,
}
impl SemanticTree {
    pub fn new(pf: &ProgramFile, sy: &RawSymbols) -> Result<Self, Vec<SemTreeBuildErrors>> {
        let mut st = SemanticTree {
            root: ProgramRoot {
                funcs: vec![],
                extern_funcs: vec![],
            },
            names: HashMap::new(),
            symbols: sy.clone(),
        };
        st.visit_program_file(pf)?;

        Ok(st)
    }
    fn visit_program_file(&mut self, pf: &ProgramFile) -> Result<(), Vec<SemTreeBuildErrors>> {
        let mut functions = vec![];
        let mut extern_functions = vec![];
        let mut errors = vec![];
        for t in &pf.declarations {
            match t {
                crate::ast::Declaration::Function(x) => match self.visit_function_decl(x) {
                    Ok(f) => functions.push(f),
                    Err(e) => errors.push(e),
                },
                crate::ast::Declaration::ExternFunction(x) => {
                    match self.visit_extern_function_decl(x) {
                        Ok(f) => extern_functions.push(f),
                        Err(e) => errors.push(e),
                    }
                }
                crate::ast::Declaration::TypeDeclSection(x) => todo!(),
            }
        }
        if errors.is_empty() {
            self.root = ProgramRoot {
                funcs: functions,
                extern_funcs: extern_functions,
            };
            Ok(())
        } else {
            Err(errors)
        }
    }
    fn visit_function_decl(&mut self, func: &FunctionBody) -> Result<Function, SemTreeBuildErrors> {
        let mut scope = Scope::new();
        let function_args: Vec<_> = func
            .function_args
            .iter()
            .flat_map(|x| {
                x.names
                    .iter()
                    .map(|y| (Id(y.to_string()), SLPType::from_ast_type(&x.ty.ty)))
            })
            .collect();
        let errs: Vec<_> = function_args
            .iter()
            .filter_map(|x| x.1.as_ref().err())
            .collect();
        if !errs.is_empty() {
            return Err(errs.first().unwrap().clone().clone());
        }
        let return_arg = SLPType::from_ast_type(&func.return_arg.ty)?;

        let function_args: Vec<(Id, SLPType)> = function_args
            .into_iter()
            .map(|(id, ty)| (id, ty.unwrap()))
            .collect();
        for (id, ty) in &function_args {
            scope.add_variable(id, ty.clone());
        }
        scope.add_variable(&Id("Result".to_owned()), return_arg.clone());
        Ok(Function {
            function_name: Id(func.function_name.clone()),
            function_args,
            return_arg,
            body: self.visit_codeblock(&func.body, &scope)?,
            loc: func.loc,
        })
    }
    fn visit_extern_function_decl(
        &mut self,
        func: &ExternFunctionBody,
    ) -> Result<ExternFunction, SemTreeBuildErrors> {
        let function_args: Vec<_> = func
            .function_args
            .iter()
            .flat_map(|x| {
                x.names
                    .iter()
                    .map(|y| (Id(y.to_string()), SLPType::from_ast_type(&x.ty.ty)))
            })
            .collect();
        let errs: Vec<_> = function_args
            .iter()
            .filter_map(|x| x.1.as_ref().err())
            .collect();
        if !errs.is_empty() {
            return Err((*errs.first().unwrap()).clone());
        }
        let return_arg = SLPType::from_ast_type(&func.return_arg.ty)?;

        let function_args: Vec<(Id, SLPType)> = function_args
            .into_iter()
            .map(|(id, ty)| (id, ty.unwrap()))
            .collect();

        Ok(ExternFunction {
            function_name: Id(func.function_name.clone()),
            function_args,
            return_arg,
            loc: func.loc,
        })
    }
    fn visit_codeblock(
        &mut self,
        block: &[Statement],
        outer: &Scope,
    ) -> Result<Vec<STStatement>, SemTreeBuildErrors> {
        let mut scope = Scope::new_with_outer(outer);
        let mut stmts: Vec<STStatement> = vec![];
        for st in block {
            stmts.append(&mut self.visit_statement(st, &mut scope)?);
        }
        Ok(stmts)
    }
    fn visit_statement(
        &mut self,
        statement: &Statement,
        outer: &mut Scope,
    ) -> Result<Vec<STStatement>, SemTreeBuildErrors> {
        match &statement {
            Statement::CodeBlock(l, b) => Ok(vec![STStatement::CodeBlock(
                l.clone(),
                self.visit_codeblock(&b, &outer)?,
            )]),
            Statement::Print(l, e) => Ok(vec![STStatement::Print(
                l.clone(),
                Box::new(self.visit_expression(&e, &outer)?),
            )]),
            Statement::FunctionCall(_, _) => todo!(),
            Statement::Assignment(_, _, _) => todo!(),
            Statement::If(_, _, _, _) => todo!(),
            Statement::While(_, _, _) => todo!(),
            Statement::RepeatUntil(_, _, _) => todo!(),
            Statement::VarDecl(l, t) => {
                self.visit_vardelc(t, l)
            },
            Statement::Empty() => Ok(vec![STStatement::Empty()]),
        }
    }
    fn visit_vardelc(&mut self, vd: &ast::VarDecl, l: &Loc) -> Result<Vec<STStatement>, SemTreeBuildErrors> {
        match vd {
            ast::VarDecl::Multiple(s, ty) => {
                let ty = SLPType::from_ast_type(&ty.ty)?;
                Ok(s.iter().map(|x|STStatement::VarDecl(*l, VarDecl { id: Id(x.clone()), ty: ty.clone(), init_expr: None })).collect())
            },
            ast::VarDecl::ExplicitType(_, _, _) => todo!(),
            ast::VarDecl::ImplicitType(_, _) => todo!(),
        }
    }
    fn visit_expression(
        &mut self,
        expr: &Expr,
        scope: &Scope,
    ) -> Result<STExpr, SemTreeBuildErrors> {
        Ok(match expr {
            Expr::Constant(l, c) => match c {
                crate::ast::Constant::String(_) => todo!(),
                crate::ast::Constant::Int64(lit) => STExpr {
                    ret_type: SLPType::PrimitiveType(crate::types::SLPPrimitiveType::Int64),
                    loc: l.clone(),
                    kind: ExprKind::NumberLiteral(NumberLiteral::I64(*lit)),
                },
                crate::ast::Constant::Float64(_) => todo!(),
                crate::ast::Constant::Bool(_) => todo!(),
            },
            Expr::Ident(_, _) => {
                todo!()
            }
            Expr::OpBinPlus(_, _, _) => todo!(),
            Expr::OpBinMinus(_, _, _) => todo!(),
            Expr::OpBinAsterisk(_, _, _) => todo!(),
            Expr::OpBinSlash(_, _, _) => todo!(),
            Expr::OpBinDiv(_, _, _) => todo!(),
            Expr::OpBinMod(_, _, _) => todo!(),
            Expr::OpUnPlus(_, _) => todo!(),
            Expr::OpUnMinus(_, _) => todo!(),
            Expr::OpBinAnd(_, _, _) => todo!(),
            Expr::OpBinOr(_, _, _) => todo!(),
            Expr::OpBinXor(_, _, _) => todo!(),
            Expr::OpUnNot(_, _) => todo!(),
            Expr::OpBinShl(_, _, _) => todo!(),
            Expr::OpBinShr(_, _, _) => todo!(),
            Expr::OpBinLesser(_, _, _) => todo!(),
            Expr::OpBinGreater(_, _, _) => todo!(),
            Expr::OpBinLesserEq(_, _, _) => todo!(),
            Expr::OpBinGreaterEq(_, _, _) => todo!(),
            Expr::OpBinEq(_, _, _) => todo!(),
            Expr::OpBinNotEq(_, _, _) => todo!(),
            Expr::OpUnDeref(_, _) => todo!(),
            Expr::OpUnGetRef(_, _) => todo!(),
            Expr::OpBinIndex(_, _, _) => todo!(),
            Expr::OpFunctionCall(_, _) => todo!(),
            Expr::OpUnAs(_, _, _) => todo!(),
            Expr::OpMethodCall(_, _, _) => todo!(),
            Expr::OpNew(_, _, _) => todo!(),
        })
    }
}

#[derive(Debug, Clone)]
struct Scope<'a> {
    outer_scope: Option<&'a Scope<'a>>,
    local_variables: HashMap<Id, Vec<(LocalVariable, SLPType)>>,
    order: Vec<LocalVariable>,
}
impl<'a> Scope<'a> {
    pub fn new() -> Self {
        Scope {
            outer_scope: None,
            local_variables: Default::default(),
            order: Default::default(),
        }
    }
    pub fn new_with_outer<'b: 'a>(outer: &'b Scope<'b>) -> Self {
        Scope {
            outer_scope: Some(outer),
            local_variables: Default::default(),
            order: Default::default(),
        }
    }
    fn rec_occupied(&self, lv: &LocalVariable) -> bool {
        self.order.contains(lv)
            || self
                .outer_scope
                .map(|x| x.rec_occupied(lv))
                .unwrap_or(false)
    }
    //Перекрывающиеся области видимости
    pub fn add_variable(&mut self, tree_id: &Id, ty: SLPType) -> LocalVariable {
        let candidate_name = LocalVariable(tree_id.0.clone());
        let mut testing_name = candidate_name.clone();
        let mut counter = 0;
        while self.rec_occupied(&testing_name) {
            testing_name = LocalVariable(format!("{}_{}", candidate_name.0, counter));
            counter += 1;
        }
        self.order.push(testing_name.clone());
        if let Some(m) = self.local_variables.get_mut(tree_id) {
            m.push((testing_name.clone(), ty))
        } else {
            self.local_variables
                .insert(tree_id.clone(), vec![(testing_name.clone(), ty)]);
        }
        testing_name
    }
    fn rec_get(&self, tree_id: &Id) -> Option<(LocalVariable, SLPType)> {
        self.local_variables
            .get(tree_id)
            .map(|x| x.last().unwrap().clone())
            .or(self.outer_scope.map_or(None, |x| x.rec_get(tree_id)))
    }
    pub fn get_variable(&self, tree_id: &Id) -> Option<(LocalVariable, SLPType)> {
        self.rec_get(tree_id)
    }
}

#[derive(Debug, Clone)]
pub struct ProgramRoot {
    pub funcs: Vec<Function>,
    pub extern_funcs: Vec<ExternFunction>,
}
#[derive(Debug, Clone)]
pub struct Function {
    pub function_name: Id,
    pub function_args: Vec<(Id, SLPType)>,
    pub return_arg: SLPType,
    pub body: Vec<STStatement>,
    pub loc: Loc,
}
#[derive(Debug, Clone)]
pub struct ExternFunction {
    pub function_name: Id,
    pub function_args: Vec<(Id, SLPType)>,
    pub return_arg: SLPType,
    pub loc: Loc,
}
#[derive(Debug, Clone)]
pub enum STStatement {
    CodeBlock(Loc, Vec<STStatement>),
    Print(Loc, Box<STExpr>),
    FunctionCall(Loc, FunctionCall),
    //RHS, LHS
    Assignment(Loc, Box<STExpr>, Box<STExpr>),
    If(Loc, Box<STExpr>, Box<STStatement>, Option<Box<STStatement>>),
    While(Loc, Box<STExpr>, Box<STStatement>),
    RepeatUntil(Loc, Box<STExpr>, Box<STStatement>),
    //Expand single declaration in multiple varDecl
    VarDecl(Loc, VarDecl),
    Empty(),
}
#[derive(Debug, Clone)]
pub struct VarDecl {
    pub id: Id,
    pub ty: SLPType,
    pub init_expr: Option<STExpr>,
}
#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub func: Box<STExpr>,
    pub args: Vec<STExpr>,
    pub ret_type: SLPType,
}
#[derive(Debug, Clone)]
pub struct STExpr {
    pub ret_type: SLPType,
    pub loc: Loc,
    pub kind: ExprKind,
}
#[derive(Debug, Clone)]
pub enum ExprKind {
    LocalVariable(LocalVariable),
    TypeCast(Box<STExpr>),
    NumberLiteral(NumberLiteral),
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LocalVariable(String);
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum NumberLiteral {
    U32(u32),
    I32(i32),
    U64(u64),
    I64(i64),
}
