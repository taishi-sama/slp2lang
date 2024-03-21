use std::{collections::HashMap, sync::Arc};

use crate::{
    ast::{self, Expr, ExternFunctionBody, FunctionBody, Loc, ProgramFile, Statement},
    errors::SemTreeBuildErrors,
    symbols::{self, ContextSymbolResolver, Id, RawSymbols},
    types::{SLPPrimitiveType, SLPType},
};
#[derive(Debug, Clone)]
pub struct SemanticTree {
    pub semtree_name: Id,
    //Replace when supporting compilation of many files
    pub symbols: ContextSymbolResolver,
    pub root: ProgramRoot,
    //Переменные пересекающихся областей определения переименовываются.
    pub names: HashMap<Id, String>,
}
impl SemanticTree {
    pub fn new(pf: &ProgramFile, ctxsy: ContextSymbolResolver, name: Id) -> Result<Self, Vec<SemTreeBuildErrors>> {
        let mut st = SemanticTree {

            names: HashMap::new(),
            symbols: ctxsy,
            root: ProgramRoot { funcs: vec![], extern_funcs: vec![] },
            semtree_name: name,
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
    fn check_implicit_convertion(from: &SLPType, to: &SLPType) -> Result<TypeConversionKind, SemTreeBuildErrors> {
        if from == to {Ok(TypeConversionKind::Identity)}
        else {todo!("Implement implicit conversion hierarcy: {:?} to {:?}", from, to)}
    }
    fn insert_impl_conversion(expr: STExpr, to: &SLPType) -> Result<STExpr, SemTreeBuildErrors> {
        match Self::check_implicit_convertion(&expr.ret_type, to)? {
            TypeConversionKind::Identity => return Ok(expr),
            TypeConversionKind::Int64ToInt32 => todo!(),
            TypeConversionKind::Int64ToInt16 => todo!(),
            TypeConversionKind::Int64ToInt8 => todo!(),
            TypeConversionKind::Int32ToInt64 => todo!(),
            TypeConversionKind::Int32ToInt16 => todo!(),
            TypeConversionKind::Int32ToInt8 => todo!(),
            TypeConversionKind::Int16ToInt64 => todo!(),
            TypeConversionKind::Int16ToInt32 => todo!(),
            TypeConversionKind::Int16ToInt8 => todo!(),
            TypeConversionKind::Int8ToInt64 => todo!(),
            TypeConversionKind::Int8ToInt32 => todo!(),
            TypeConversionKind::Int8ToInt16 => todo!(),
        }
    }
    fn check_kind_of_function(&mut self, loc: Loc, fc: &ast::FunctionCall, scope: &Scope) -> Result<FunctionCallResolveResult, SemTreeBuildErrors> {
        let mut args = vec![];
        for a in &fc.args {
            let expr = self.visit_expression(a, scope)?;
            args.push(expr);
        }
        if let ast::Expr::Ident(l, id) = fc.func.as_ref() {
             //TODO full path resolve
            let t = self.symbols.resolve(id)?;
            match t {
                Some((id, func)) => {
                    match func {
                        crate::symbols::RawSymbol::FunctionDecl { loc, input, output } => {
                            let mut reconst_exprs = vec![];
                            for (inp_type, expr) in input.iter().zip(args.into_iter()) {
                                let res = Self::insert_impl_conversion(expr, inp_type)?;
                                reconst_exprs.push(res);
                            }
                            return Ok(FunctionCallResolveResult::FunctionCall(FunctionCall { func: id, args: reconst_exprs, ret_type: output.clone() }));
                        },
                        crate::symbols::RawSymbol::ExternFunctionDecl { loc, input, output } => {
                            let mut reconst_exprs = vec![];
                            for (inp_type, expr) in input.iter().zip(args.into_iter()) {
                                let res = Self::insert_impl_conversion(expr, inp_type)?;
                                reconst_exprs.push(res);
                            }
                            return Ok(FunctionCallResolveResult::FunctionCall(FunctionCall { func: id, args: reconst_exprs, ret_type: output.clone() }));
                        },
                    }
                },
                None => {
                    todo!("Type cast parse")
                },
            }
        }
        else {
            todo!("Only direct function calls supported, loc: {}", loc)
        }
        todo!()
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
            Statement::FunctionCall(l, func) => {
                match self.check_kind_of_function(*l, func, outer)? {
                    FunctionCallResolveResult::FunctionCall(f) => 
                    Ok(vec![STStatement::FunctionCall(l.clone(), f)]),
                    FunctionCallResolveResult::TypeCast { from, ret_type } => todo!(),
                } 
            },
            Statement::Assignment(l, target, from) => {
                let target = self.visit_expression(&target, &outer)?;
                let from = self.visit_expression(&from, &outer)?;
                let from = Self::insert_impl_conversion(from, &target.ret_type)?;
                Ok(vec![STStatement::Assignment(*l, Box::new(target), Box::new(from))])
            },
            Statement::If(l, cond, mb, ab) => {
                let cond = self.visit_expression(cond, outer)?;
                let cond = Self::insert_impl_conversion(cond, &SLPType::PrimitiveType(SLPPrimitiveType::Bool))?;
                let mbstmt = {
                    let mut first_scope = Scope::new_with_outer(outer);
                    Box::new(STStatement::CodeBlock(*l, self.visit_statement(&mb, &mut first_scope)?))
                };
                let abstmt = {
                    if let Some(stmt) = ab {
                        let mut first_scope = Scope::new_with_outer(outer);
                        Some(Box::new(STStatement::CodeBlock(*l, self.visit_statement(&stmt, &mut first_scope)?)))
                    }
                    else {
                        None
                    }
                };
                Ok(vec![STStatement::If(*l, Box::new(cond), mbstmt,  abstmt)])
            },
            Statement::While(l, cond, stmt) => {
                let cond = self.visit_expression(cond, outer)?;
                let cond = Self::insert_impl_conversion(cond, &SLPType::PrimitiveType(SLPPrimitiveType::Bool))?;
                let mbstmt = {
                    let mut first_scope = Scope::new_with_outer(outer);
                    Box::new(STStatement::CodeBlock(*l, self.visit_statement(&stmt, &mut first_scope)?))
                };
                Ok(vec![STStatement::While(*l, Box::new(cond), mbstmt)])
            },
            Statement::RepeatUntil(_, _, _) => todo!(),
            Statement::VarDecl(l, t) => {
                self.visit_vardelc(t, l, outer)
            },
            Statement::Empty() => Ok(vec![STStatement::Empty()]),
        }
    }
    fn visit_vardelc(&mut self, vd: &ast::VarDecl, l: &Loc,  scope: &mut Scope) -> Result<Vec<STStatement>, SemTreeBuildErrors> {
        match vd {
            ast::VarDecl::Multiple(s, ty) => {
                let ty = SLPType::from_ast_type(&ty.ty)?;
                let mut lvs = vec![];
                for i in s {
                    let lv = scope.add_variable(&Id(i.clone()), ty.clone());
                    let t = STStatement::VarDecl(*l, VarDecl { id: lv, ty: ty.clone(), init_expr: None });
                    lvs.push(t)
                }
                Ok(lvs)
            },
            ast::VarDecl::ExplicitType(s, ty, e) => {
                let ty = SLPType::from_ast_type(&ty.ty)?;
                let lv = scope.add_variable(&Id(s.clone()), ty.clone());
                Ok(vec![STStatement::VarDecl(*l, VarDecl { id: lv, ty, init_expr: Some(self.visit_expression(e, scope)?) })])
            },
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

                
                ast::Constant::String(_) => todo!(),
                ast::Constant::Int64(lit) => STExpr {
                    ret_type: SLPType::PrimitiveType(crate::types::SLPPrimitiveType::Int64),
                    loc: l.clone(),
                    kind: ExprKind::NumberLiteral(NumberLiteral::I64(*lit)),
                },
                ast::Constant::Int32(lit) => STExpr {
                    ret_type: SLPType::PrimitiveType(crate::types::SLPPrimitiveType::Int32),
                    loc: l.clone(),
                    kind: ExprKind::NumberLiteral(NumberLiteral::I32(*lit)),
                },
                ast::Constant::Int16(lit) => STExpr {
                    ret_type: SLPType::PrimitiveType(crate::types::SLPPrimitiveType::Int16),
                    loc: l.clone(),
                    kind: ExprKind::NumberLiteral(NumberLiteral::I16(*lit)),
                },
                ast::Constant::Int8(lit) => STExpr {
                    ret_type: SLPType::PrimitiveType(crate::types::SLPPrimitiveType::Int8),
                    loc: l.clone(),
                    kind: ExprKind::NumberLiteral(NumberLiteral::I8(*lit)),
                },
                ast::Constant::Float64(_) => todo!(),
                ast::Constant::Bool(b) => STExpr {
                    ret_type: SLPType::PrimitiveType(crate::types::SLPPrimitiveType::Bool),
                    loc: l.clone(),
                    kind: ExprKind::BoolLiteral(*b)
                },
                
            },
            Expr::Ident(l, i) => {
                let id = Id(i.name.clone());
                if let Some((lv, ty)) = scope.get_variable(&id) {
                    STExpr{ ret_type: ty, loc: *l, kind: ExprKind::LocalVariable(lv) }
                }
                else {
                    todo!()
                }
                
            }
            Expr::OpBinPlus(l, x, y) => {
                self.visit_int_bin_op(*l, &x, &y, scope, IntBinOp::Add)?
            },
            Expr::OpBinMinus(l, x, y) => {
                self.visit_int_bin_op(*l, &x, &y, scope, IntBinOp::Substract)?
            },
            Expr::OpBinAsterisk(l, x, y) => {
                self.visit_int_bin_op(*l, &x, &y, scope, IntBinOp::Multiplication)?
            },
            Expr::OpBinSlash(l, x, y) => {
                self.visit_int_bin_op(*l, &x, &y, scope, IntBinOp::Division)?
            },
            Expr::OpBinDiv(_, _, _) => todo!(),
            Expr::OpBinMod(l, x, y) => {
                self.visit_int_bin_op(*l, &x, &y, scope, IntBinOp::Modulo)?
            },
            Expr::OpUnPlus(_, _) => todo!(),
            Expr::OpUnMinus(_, _) => todo!(),
            Expr::OpBinAnd(l, x, y) => {
                self.visit_int_bin_op(*l, &x, &y, scope, IntBinOp::And)?
            },
            Expr::OpBinOr(l, x, y) => {
                self.visit_int_bin_op(*l, &x, &y, scope, IntBinOp::Or)?
            },
            Expr::OpBinXor(l, x, y) => {
                self.visit_int_bin_op(*l, &x, &y, scope, IntBinOp::Xor)?
            },
            Expr::OpUnNot(_, _) => todo!(),
            Expr::OpBinShl(_, _, _) => todo!(),
            Expr::OpBinShr(_, _, _) => todo!(),
            Expr::OpBinLesser(l, x, y) => {
                self.visit_int_comparations(*l, &x, &y, scope, ComparationKind::LesserThan)?
            },
            Expr::OpBinGreater(l, x, y) => {
                self.visit_int_comparations(*l, &x, &y, scope, ComparationKind::GreaterThan)?
            },
            Expr::OpBinLesserEq(l, x, y) => {
                self.visit_int_comparations(*l, &x, &y, scope, ComparationKind::LesserEqual)?
            },
            Expr::OpBinGreaterEq(l, x, y) => {
                self.visit_int_comparations(*l, &x, &y, scope, ComparationKind::GreaterEqual)?
            },
            Expr::OpBinEq(l, x, y) => {
                self.visit_int_comparations(*l, &x, &y, scope, ComparationKind::Equal)?
            },
            Expr::OpBinNotEq(l, x, y) => {
                self.visit_int_comparations(*l, &x, &y, scope, ComparationKind::NotEqual)?
            },
            Expr::OpUnDeref(_, _) => todo!(),
            Expr::OpUnGetRef(_, _) => todo!(),
            Expr::OpBinIndex(_, _, _) => todo!(),
            Expr::OpFunctionCall(l, func) => match self.check_kind_of_function(*l, func, scope)? {
                FunctionCallResolveResult::FunctionCall(f) => 
                STExpr{ret_type: f.ret_type.clone(), loc: l.clone(), kind: ExprKind::FunctionCall(f) },
                FunctionCallResolveResult::TypeCast { from, ret_type } => todo!(),
            } ,
            Expr::OpUnAs(_, _, _) => todo!(),
            Expr::OpMethodCall(_, _, _) => todo!(),
            Expr::OpNew(_, _, _) => todo!(),
        })
    }
    fn visit_int_bin_op(&mut self,
        loc: Loc, l: &Expr, r: &Expr,
        scope: &Scope, kind: IntBinOp) -> Result<STExpr, SemTreeBuildErrors> {
        let le = Box::new(self.visit_expression(&l, scope)?);
        let re = Box::new(self.visit_expression(&r, scope)?);
        if le.ret_type == re.ret_type {
            if le.ret_type.is_int() {
                Ok(STExpr{ret_type: le.ret_type.clone(), loc, kind: ExprKind::PrimitiveIntBinOp(le, re, kind) })
            }
            else if le.ret_type.is_bool() {
                let bool_op = match kind {

                    IntBinOp::Or => BoolBinOp::Or,
                    IntBinOp::And => BoolBinOp::And,
                    IntBinOp::Xor => BoolBinOp::Xor,

                    _ => todo!("Proper error handling")
                };
                Ok(STExpr{ret_type: SLPType::PrimitiveType(SLPPrimitiveType::Bool), loc, kind: ExprKind::BoolBinOp(le, re, bool_op)})
            }
            else {
                todo!("Type error!");
            }
        }
        else {
            todo!("Non-equal type conversion hierarchy from {:?} to {:?}", le.ret_type, re.ret_type)
        }
    }
    fn visit_int_comparations(&mut self,
        loc: Loc, l: &Expr, r: &Expr,
        scope: &Scope, kind: ComparationKind) -> Result<STExpr, SemTreeBuildErrors> {
            let le = Box::new(self.visit_expression(&l, scope)?);
            let re = Box::new(self.visit_expression(&r, scope)?);
            if le.ret_type == re.ret_type {
                if le.ret_type.is_int() {
                    Ok(STExpr{ret_type: SLPType::PrimitiveType(SLPPrimitiveType::Bool), loc, kind: ExprKind::PrimitiveIntComparation(le, re, kind) })
                }
                else if le.ret_type.is_bool() {
                    let bool_op = match kind {
                        ComparationKind::Equal => BoolBinOp::Equal,
                        ComparationKind::NotEqual => BoolBinOp::NotEqual,
                        _ => todo!("Proper error handling")
                    };
                    Ok(STExpr{ret_type: SLPType::PrimitiveType(SLPPrimitiveType::Bool), loc, kind: ExprKind::BoolBinOp(le, re, bool_op)})
                }
                else {
                    todo!("Type error!");
                }
            }
            else {
                todo!("Non-equal type conversion hierarchy")
            }
    }
    fn visit_int_unary_op(&mut self,
        loc: Loc, i: &Expr,
        scope: &Scope, kind: IntUnaryOp) -> Result<STExpr, SemTreeBuildErrors> {
            let inp = Box::new(self.visit_expression(&i, scope)?);
            if inp.ret_type.is_int() {
                Ok(STExpr{ret_type: SLPType::PrimitiveType(SLPPrimitiveType::Bool), loc, kind: ExprKind::PrimitiveIntUnaryOp(inp, kind) })
            }
            else if inp.ret_type.is_bool() {
                let bool_op = match kind {

                    IntUnaryOp::Inverse => BoolUnaryOp::Not,

                    _ => todo!("Proper error handling")
                };
                Ok(STExpr{ret_type: SLPType::PrimitiveType(SLPPrimitiveType::Bool), loc, kind: ExprKind::BoolUnaryOp(inp, bool_op)})
            }
            else {
                todo!("Type error")
            }
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
        //Rename all variables in scope of function
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
    pub id: LocalVariable,
    pub ty: SLPType,
    pub init_expr: Option<STExpr>,
}
#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub func: Id,
    pub args: Vec<STExpr>,
    pub ret_type: SLPType,
}
#[derive(Debug, Clone)]
pub enum FunctionCallResolveResult {
    FunctionCall(FunctionCall),
    TypeCast{
        from: Box<STExpr>,
        ret_type: SLPType,
    }
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
    BoolLiteral(bool),
    FunctionCall(FunctionCall),
    PrimitiveIntBinOp(Box<STExpr>, Box<STExpr>, IntBinOp),
    PrimitiveIntUnaryOp(Box<STExpr>, IntUnaryOp),
    PrimitiveIntComparation(Box<STExpr>, Box<STExpr>, ComparationKind),
    BoolBinOp(Box<STExpr>, Box<STExpr>, BoolBinOp),
    BoolUnaryOp(Box<STExpr>, BoolUnaryOp),

}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalVariable(pub String);
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum NumberLiteral {
    I64(i64),
    I32(i32),
    I16(i16),
    I8(i8),
    U64(u64),    
    U32(u32),
    U16(u16),
    U8(u8),
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ComparationKind {
    LesserThan,
    LesserEqual,
    GreaterThan,
    GreaterEqual,
    Equal,
    NotEqual,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntBinOp {
    Add,
    Substract, 
    Multiplication,
    Division,
    Modulo,
    Or,
    And,
    Xor,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntUnaryOp {
    Minus,
    Inverse,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BoolBinOp {
    And,
    Or,
    Xor,
    Equal,
    NotEqual
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BoolUnaryOp {
    Not
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeConversionKind {
    Identity,
    Int64ToInt32,
    Int64ToInt16,
    Int64ToInt8,
    Int32ToInt64,
    Int32ToInt16,
    Int32ToInt8,
    Int16ToInt64,
    Int16ToInt32,
    Int16ToInt8,
    Int8ToInt64,
    Int8ToInt32,
    Int8ToInt16,
}
