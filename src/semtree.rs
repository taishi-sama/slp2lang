use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
    sync::Arc,
};

use crate::{
    ast::{self, Expr, ExternFunctionBody, FunctionBody, Loc, ProgramFile, Statement},
    compiler::FileId,
    errors::SemTreeBuildErrors,
    symbols::{ContextSymbolResolver, Id, Symbols, TypeSymbolResolver},
    types::{SLPPrimitiveType, SLPType},
};
#[derive(Debug, Clone)]
pub struct SemanticTree {
    pub semtree_name: Id,
    pub fileid: FileId,
    //Replace when supporting compilation of many files
    pub symbols: ContextSymbolResolver,
    pub types_resolver: Arc<TypeSymbolResolver>,
    pub root: ProgramRoot,
    //Переменные пересекающихся областей определения переименовываются.
    pub names: HashMap<Id, String>,
}
impl SemanticTree {
    pub fn new(
        pf: &ProgramFile,
        ctxsy: ContextSymbolResolver,
        name: Id,
        fileid: FileId,
        ty_res: Arc<TypeSymbolResolver>,
    ) -> Result<Self, Vec<SemTreeBuildErrors>> {
        let mut st = SemanticTree {
            names: HashMap::new(),
            symbols: ctxsy,
            root: ProgramRoot {
                funcs: vec![],
                extern_funcs: vec![],
            },
            semtree_name: name,
            types_resolver: ty_res,
            fileid,
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
                crate::ast::Declaration::TypeDeclSection(_x) => {
                    //TODO
                }
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
        let return_arg = self
            .types_resolver
            .from_ast_type(&func.return_arg.ty, &self.fileid)?;

        let function_args: Vec<(Id, SLPType)> =
            Symbols::convert_typedecls(&self.fileid, &func.function_args, &self.types_resolver)?;

        for (id, ty) in &function_args {
            scope.add_variable(id, ty.clone());
        }
        scope.add_variable(&Id("Result".to_owned()), return_arg.clone());
        let visit_result = self.visit_codeblock(&func.body, &scope)?;
        let temp_variables = scope.temporary_variables.as_ref().borrow().clone();
        Ok(Function {
            function_name: Id(func.function_name.clone()),
            function_args,
            return_arg,
            body: visit_result,
            loc: func.loc,
            temporary_variables: temp_variables,
        })
    }
    fn visit_extern_function_decl(
        &mut self,
        func: &ExternFunctionBody,
    ) -> Result<ExternFunction, SemTreeBuildErrors> {
        let return_arg = self
            .types_resolver
            .from_ast_type(&func.return_arg.ty, &self.fileid)?;

        let function_args: Vec<(Id, SLPType)> =
            Symbols::convert_typedecls(&self.fileid, &func.function_args, &self.types_resolver)?;

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

    fn try_get_autoref(expr: STExpr) -> Result<STExpr, SemTreeBuildErrors> {
        if let ExprKind::LocalVariable(lv) = expr.kind {
            let ty = SLPType::AutoderefPointer(Box::new(expr.ret_type));
            let res = STExpr {
                ret_type: ty,
                loc: expr.loc.clone(),
                kind: ExprKind::GetLocalVariableRef(lv),
            };
            Ok(res)
        } else {
            todo!()
        }
    }
    fn insert_autoderef_or_pass(expr: STExpr) -> Result<STExpr, SemTreeBuildErrors> {
        if let Some(ty) = expr.ret_type.get_underlying_autoderef_type() {
            let target = ty.clone();
            Ok(STExpr {
                ret_type: target,
                loc: expr.loc.clone(),
                kind: ExprKind::Deref(Box::new(expr)),
            })
        } else {
            Ok(expr)
        }
    }
    fn insert_impl_conversion(expr: STExpr, to: &SLPType) -> Result<STExpr, SemTreeBuildErrors> {
        let from = &expr.ret_type;
        if from == to {
            Ok(expr)
        } else if let Some(ty) = from.get_underlying_autoderef_type() {
            if ty == to {
                Ok(STExpr {
                    ret_type: to.clone(),
                    loc: expr.loc.clone(),
                    kind: ExprKind::Deref(Box::new(expr)),
                })
            } else {
                todo!(
                    "Implement implicit conversion hierarcy: {:?} to {:?}",
                    from,
                    to
                )
            }
        } else if let Some(ty) = to.get_underlying_autoderef_type() {
            if ty == from {
                Self::try_get_autoref(expr)
            } else {
                todo!(
                    "Implement implicit conversion hierarcy: {:?} to {:?}",
                    from,
                    to
                )
            }
        } else {
            todo!(
                "Implement implicit conversion hierarcy: {:?} to {:?}",
                from,
                to
            )
        }

    }
    fn check_kind_of_function(
        &mut self,
        loc: Loc,
        fc: &ast::FunctionCall,
        scope: &Scope,
    ) -> Result<FunctionCallResolveResult, SemTreeBuildErrors> {
        let mut args = vec![];
        for a in &fc.args {
            let expr = self.visit_expression(a, scope)?;
            args.push(expr);
        }
        if let ast::Expr::Ident(_l, id) = fc.func.as_ref() {
            //TODO full path resolve
            let t = self.symbols.resolve(id)?;
            match t {
                Some((id, func)) => match func {
                    crate::symbols::FunctionDecl::FunctionDecl { loc: _loc, input, output } => {
                        let mut reconst_exprs = vec![];
                        for (inp_type, expr) in input.iter().zip(args.into_iter()) {
                            let res = Self::insert_impl_conversion(expr, inp_type)?;
                            reconst_exprs.push(res);
                        }
                        return Ok(FunctionCallResolveResult::FunctionCall(FunctionCall {
                            func: id,
                            args: reconst_exprs,
                            ret_type: output.clone(),
                        }));
                    }
                    crate::symbols::FunctionDecl::ExternFunctionDecl { loc: _loc, input, output } => {
                        let mut reconst_exprs = vec![];
                        for (inp_type, expr) in input.iter().zip(args.into_iter()) {
                            let res = Self::insert_impl_conversion(expr, inp_type)?;
                            reconst_exprs.push(res);
                        }
                        return Ok(FunctionCallResolveResult::FunctionCall(FunctionCall {
                            func: id,
                            args: reconst_exprs,
                            ret_type: output.clone(),
                        }));
                    }
                },
                None => {
                    //TODO check if symbol is type instead of assuming this
                    let ty = ast::Type::Primitive(id.clone());
                    if args.len() == 1 {
                        return Ok(FunctionCallResolveResult::TypeCast(self.resolve_typecast(
                            &ty,
                            args.pop().unwrap(),
                            loc,
                        )?));
                    } else {
                        todo!("Invalid amount of arguments report")
                    }
                }
            }
        } else {
            todo!("Only direct function calls supported, loc: {}", loc)
        }
    }
    fn resolve_typecast(
        &self,
        ty: &ast::Type,
        input_expr: STExpr,
        loc: Loc,
    ) -> Result<STExpr, SemTreeBuildErrors> {
        let target = self.types_resolver.from_ast_type(&ty, &self.fileid)?;

        let source = &input_expr.ret_type;
        let tck = if &target == source {
            TypeConversionKind::Identity
        } else if source.is_unsigned_int() && target.is_unsigned_int() {
            if source.get_number_size().unwrap() > target.get_number_size().unwrap() {
                TypeConversionKind::UnsignedIntTruncate
            } else {
                TypeConversionKind::UnsignedIntExtend
            }
        } else if source.is_any_int() && target.is_unsigned_int() {
            if source.get_number_size().unwrap() > target.get_number_size().unwrap() {
                TypeConversionKind::SignedToUnsignedTruncate
            } else {
                TypeConversionKind::SignedToUnsignedExtend
            }
        } else if source.is_unsigned_int() && target.is_any_int() {
            if source.get_number_size().unwrap() > target.get_number_size().unwrap() {
                TypeConversionKind::UnsignedToSignedTruncate
            } else {
                TypeConversionKind::UnsignedToSignedExtend
            }
        } else if source.is_any_int() && target.is_any_int() {
            if source.get_number_size().unwrap() > target.get_number_size().unwrap() {
                TypeConversionKind::SignedIntTruncate
            } else {
                TypeConversionKind::SignedIntExtend
            }
        } else if source.is_char() && target.is_any_int() {
            if source.get_number_size().unwrap() > target.get_number_size().unwrap() {
                TypeConversionKind::UnsignedIntTruncate
            } else {
                TypeConversionKind::UnsignedIntExtend
            }
        } else if source.is_any_int() && target.is_char() {
            if source.get_number_size().unwrap() > target.get_number_size().unwrap() {
                TypeConversionKind::UnsignedIntTruncate
            } else {
                TypeConversionKind::UnsignedIntExtend
            }
        } else {
            todo!()
        };
        return Ok(STExpr {
            ret_type: target,
            loc,
            kind: ExprKind::TypeCast(Box::new(input_expr), tck),
        });
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
            Statement::FunctionCall(l, func) => {
                match self.check_kind_of_function(*l, func, outer)? {
                    FunctionCallResolveResult::FunctionCall(f) => {
                        Ok(vec![STStatement::FunctionCall(l.clone(), f)])
                    }
                    FunctionCallResolveResult::TypeCast(_) => {
                        todo!("Report proper error about wrong context")
                    }
                }
            }
            Statement::Assignment(l, target, from) => {
                let target = self.visit_rhs_expression(&target, &outer)?;
                let from = self.visit_expression(&from, &outer)?;
                let from = Self::insert_impl_conversion(from, &target.required_type)?;
                Ok(vec![STStatement::Assignment(
                    *l,
                    Box::new(target),
                    Box::new(from),
                )])
            }
            Statement::If(l, cond, mb, ab) => {
                let cond = self.visit_expression(cond, outer)?;
                let cond = Self::insert_impl_conversion(
                    cond,
                    &SLPType::PrimitiveType(SLPPrimitiveType::Bool),
                )?;
                let mbstmt = {
                    let mut first_scope = Scope::new_with_outer(outer);
                    Box::new(STStatement::CodeBlock(
                        *l,
                        self.visit_statement(&mb, &mut first_scope)?,
                    ))
                };
                let abstmt = {
                    if let Some(stmt) = ab {
                        let mut first_scope = Scope::new_with_outer(outer);
                        Some(Box::new(STStatement::CodeBlock(
                            *l,
                            self.visit_statement(&stmt, &mut first_scope)?,
                        )))
                    } else {
                        None
                    }
                };
                Ok(vec![STStatement::If(*l, Box::new(cond), mbstmt, abstmt)])
            }
            Statement::While(l, cond, stmt) => {
                let cond = self.visit_expression(cond, outer)?;
                let cond = Self::insert_impl_conversion(
                    cond,
                    &SLPType::PrimitiveType(SLPPrimitiveType::Bool),
                )?;
                let mbstmt = {
                    let mut first_scope = Scope::new_with_outer(outer);
                    Box::new(STStatement::CodeBlock(
                        *l,
                        self.visit_statement(&stmt, &mut first_scope)?,
                    ))
                };
                Ok(vec![STStatement::While(*l, Box::new(cond), mbstmt)])
            }
            Statement::RepeatUntil(_, _, _) => todo!(),
            Statement::VarDecl(l, t) => self.visit_vardelc(t, l, outer),
            Statement::Empty() => Ok(vec![STStatement::Empty()]),
        }
    }
    fn visit_vardelc(
        &mut self,
        vd: &ast::VarDecl,
        l: &Loc,
        scope: &mut Scope,
    ) -> Result<Vec<STStatement>, SemTreeBuildErrors> {
        match vd {
            ast::VarDecl::Multiple(s, ty) => {
                let ty = self.types_resolver.from_ast_type(&ty.ty, &self.fileid)?;
                let mut lvs = vec![];
                for i in s {
                    let lv = scope.add_variable(&Id(i.clone()), ty.clone());
                    let t = STStatement::VarDecl(
                        *l,
                        VarDecl {
                            id: lv,
                            ty: ty.clone(),
                            init_expr: None,
                        },
                    );
                    lvs.push(t)
                }
                Ok(lvs)
            }
            ast::VarDecl::ExplicitType(s, ty, e) => {
                let ty = self.types_resolver.from_ast_type(&ty.ty, &self.fileid)?;
                let expr = self.visit_expression(e, scope)?;
                let converted_expr = Self::insert_impl_conversion(expr, &ty)?;
                let lv = scope.add_variable(&Id(s.clone()), ty.clone());
                Ok(vec![STStatement::VarDecl(
                    *l,
                    VarDecl {
                        id: lv,
                        ty,
                        init_expr: Some(converted_expr),
                    },
                )])
            }
            ast::VarDecl::ImplicitType(s, e) => {
                //let ty = self.types_resolver.from_ast_type(&ty.ty, &self.fileid)?;
                let expr = self.visit_expression(e, scope)?;
                let lv = scope.add_variable(&Id(s.clone()), expr.ret_type.clone());
                Ok(vec![STStatement::VarDecl(
                    *l,
                    VarDecl {
                        id: lv,
                        ty: expr.ret_type.clone(),
                        init_expr: Some(self.visit_expression(e, scope)?),
                    },
                )])
            }
        }
    }
    fn visit_int_constant(&self, cnst: &str, l: &Loc) -> Result<STExpr, SemTreeBuildErrors> {
        //let int = Regex::new(r#"(-)?(((0b|0x|0o)[0-9a-fA-F][0-9_a-fA-F]*)|([0-9][0-9_]*))(u8|i8|u16|i16|u32|i32|u64|i64|isize|usize)?"#).unwrap();
        let minus = cnst.starts_with('-');
        let after_minus = if minus { &cnst[1..] } else { &cnst[..] };
        let base: u32;
        let after_prefix: &str;
        if after_minus.starts_with("0b") {
            base = 2;
            after_prefix = &after_minus[2..];
        } else if after_minus.starts_with("0x") {
            base = 16;
            after_prefix = &after_minus[2..];
        } else if after_minus.starts_with("0o") {
            base = 8;
            after_prefix = &after_minus[2..];
        } else {
            base = 10;
            after_prefix = &after_minus[..];
        }
        let (number, rem) = Self::parse_int(after_prefix, base)?;
        let ty = match rem {
            "i8" => STExpr {
                ret_type: SLPType::PrimitiveType(SLPPrimitiveType::Int8),
                loc: l.clone(),
                kind: ExprKind::NumberLiteral(NumberLiteral::I8({
                    let num = number.try_into().unwrap();
                    if !minus {
                        num
                    } else {
                        -num
                    }
                })),
            },
            "i16" => STExpr {
                ret_type: SLPType::PrimitiveType(SLPPrimitiveType::Int16),
                loc: l.clone(),
                kind: ExprKind::NumberLiteral(NumberLiteral::I16({
                    let num = number.try_into().unwrap();
                    if !minus {
                        num
                    } else {
                        -num
                    }
                })),
            },
            "i32" => STExpr {
                ret_type: SLPType::PrimitiveType(SLPPrimitiveType::Int32),
                loc: l.clone(),
                kind: ExprKind::NumberLiteral(NumberLiteral::I32({
                    let num = number.try_into().unwrap();
                    if !minus {
                        num
                    } else {
                        -num
                    }
                })),
            },
            "i64" => STExpr {
                ret_type: SLPType::PrimitiveType(SLPPrimitiveType::Int64),
                loc: l.clone(),
                kind: ExprKind::NumberLiteral(NumberLiteral::I64({
                    let num = number.try_into().unwrap();
                    if !minus {
                        num
                    } else {
                        -num
                    }
                })),
            },
            "isize" => todo!(),
            "u8" => STExpr {
                ret_type: SLPType::PrimitiveType(SLPPrimitiveType::Uint8),
                loc: l.clone(),
                kind: ExprKind::NumberLiteral(NumberLiteral::U8({
                    let num = number.try_into().unwrap();
                    if !minus {
                        num
                    } else {
                        todo!("Can't apply minus operator for unsigned")
                    }
                })),
            },
            "u16" => STExpr {
                ret_type: SLPType::PrimitiveType(SLPPrimitiveType::Uint16),
                loc: l.clone(),
                kind: ExprKind::NumberLiteral(NumberLiteral::U16({
                    let num = number.try_into().unwrap();
                    if !minus {
                        num
                    } else {
                        todo!("Can't apply minus operator for unsigned")
                    }
                })),
            },
            "u32" => STExpr {
                ret_type: SLPType::PrimitiveType(SLPPrimitiveType::Uint32),
                loc: l.clone(),
                kind: ExprKind::NumberLiteral(NumberLiteral::U32({
                    let num = number.try_into().unwrap();
                    if !minus {
                        num
                    } else {
                        todo!("Can't apply minus operator for unsigned")
                    }
                })),
            },
            "u64" => STExpr {
                ret_type: SLPType::PrimitiveType(SLPPrimitiveType::Uint64),
                loc: l.clone(),
                kind: ExprKind::NumberLiteral(NumberLiteral::U64({
                    let num = number.try_into().unwrap();
                    if !minus {
                        num
                    } else {
                        todo!("Can't apply minus operator for unsigned")
                    }
                })),
            },
            "usize" => todo!(),

            "" => STExpr {
                ret_type: SLPType::PrimitiveType(SLPPrimitiveType::Int32),
                loc: l.clone(),
                kind: ExprKind::NumberLiteral(NumberLiteral::I32({
                    let num = number.try_into().unwrap();
                    if !minus {
                        num
                    } else {
                        -num
                    }
                })),
            },
            _ => todo!("Can't parse suffix: {}", rem),
        };
        Ok(ty)
    }
    fn parse_int(digits: &str, base: u32) -> Result<(u64, &str), SemTreeBuildErrors> {
        let mut curr = 0;
        let mut acc = 0u64;
        for c in digits.chars() {
            let c = c.to_ascii_lowercase();
            if "01234567890abcdef".contains(c) {
                let digit = match c {
                    n @ '0'..='9' => n as u32 - '0' as u32,
                    n @ 'a'..='f' => 10 + n as u32 - 'a' as u32,
                    _ => unreachable!(),
                };
                if digit >= base {
                    todo!("Proper wrong numer base exception!")
                }
                acc = acc.checked_mul(base as u64).unwrap();
                acc = acc.checked_add(digit as u64).unwrap();
                curr += 1;
            } else if "_".contains(c) {
                curr += 1;
            } else {
                return Ok((acc, &digits[curr..]));
            }
        }
        Ok((acc, &digits[curr..]))
    }
    fn visit_char_const(&self, cnst: &str, l: &Loc) -> Result<STExpr, SemTreeBuildErrors> {
        let trimmed = &cnst[1..(cnst.len() - 1)];
        let ch: char;
        if trimmed.chars().count() == 1 {
            ch = trimmed.chars().next().unwrap()
        } else {
            if trimmed == "\\n" {
                ch = '\n'
            } else if trimmed == r"\t" {
                ch = '\t'
            } else if trimmed == r"\r" {
                ch = '\r'
            } else if trimmed == r"\\" {
                ch = '\\'
            } else if trimmed == r"\'" {
                ch = '\''
            } else if trimmed == r#"\""# {
                ch = '\"'
            } else {
                todo!("{trimmed}")
            }
        };
        Ok(STExpr {
            ret_type: SLPType::PrimitiveType(SLPPrimitiveType::Char),
            loc: l.clone(),
            kind: ExprKind::CharLiteral(ch),
        })
    }
    fn visit_indexation_expression(
        &mut self,
        index: &Expr,
        array_begin: i64,
        _array_len: u64,
        scope: &Scope,
    ) -> Result<STExpr, SemTreeBuildErrors> {
        let index = self.visit_expression(index, scope)?;
        let index = Self::insert_autoderef_or_pass(index)?;
        if index.ret_type.is_any_int() {
            let c = match &index.ret_type {
                SLPType::PrimitiveType(pt) => match pt {
                    SLPPrimitiveType::Int8 => {
                        ExprKind::NumberLiteral(NumberLiteral::I8(array_begin.try_into().unwrap()))
                    }
                    SLPPrimitiveType::Int16 => {
                        ExprKind::NumberLiteral(NumberLiteral::I16(array_begin.try_into().unwrap()))
                    }
                    SLPPrimitiveType::Int32 => {
                        ExprKind::NumberLiteral(NumberLiteral::I32(array_begin.try_into().unwrap()))
                    }
                    SLPPrimitiveType::Int64 => {
                        ExprKind::NumberLiteral(NumberLiteral::I64(array_begin.try_into().unwrap()))
                    }
                    SLPPrimitiveType::Uint8 => {
                        ExprKind::NumberLiteral(NumberLiteral::U8(array_begin.try_into().unwrap()))
                    }
                    SLPPrimitiveType::Uint16 => {
                        ExprKind::NumberLiteral(NumberLiteral::U16(array_begin.try_into().unwrap()))
                    }
                    SLPPrimitiveType::Uint32 => {
                        ExprKind::NumberLiteral(NumberLiteral::U32(array_begin.try_into().unwrap()))
                    }
                    SLPPrimitiveType::Uint64 => {
                        ExprKind::NumberLiteral(NumberLiteral::U64(array_begin.try_into().unwrap()))
                    }
                    SLPPrimitiveType::ISize => todo!(),
                    SLPPrimitiveType::USize => todo!(),
                    SLPPrimitiveType::Float32 => todo!(),
                    SLPPrimitiveType::Float64 => todo!(),
                    SLPPrimitiveType::String => todo!(),
                    SLPPrimitiveType::Char => todo!(),
                    SLPPrimitiveType::Bool => todo!(),
                    SLPPrimitiveType::Void => todo!(),
                },
                _ => todo!(),
            };
            let cons = STExpr {
                ret_type: index.ret_type.clone(),
                loc: index.loc.clone(),
                kind: c,
            };
            let ex = STExpr {
                ret_type: index.ret_type.clone(),
                loc: index.loc.clone(),
                kind: ExprKind::PrimitiveIntBinOp(
                    Box::new(index),
                    Box::new(cons),
                    IntBinOp::Substract,
                ),
            };
            Ok(ex)
        } else {
            todo!("{:?}", index.ret_type)
        }
    }
    fn visit_array_index(
        &mut self,
        indexable: &Expr,
        index: &Expr,
        scope: &Scope,
        loc: Loc,
    ) -> Result<STExpr, SemTreeBuildErrors> {
        let indexable_expr = self.visit_expression(indexable, scope)?;
        if let SLPType::FixedArray {
            size,
            index_offset,
            ty,
        } = &indexable_expr.ret_type
        {
            if let ExprKind::LocalVariable(lv) = &indexable_expr.kind {
                let index_expr =
                    self.visit_indexation_expression(index, *index_offset, *size, scope)?;
                let res = STExpr {
                    ret_type: SLPType::AutoderefPointer(ty.clone()),
                    loc,
                    kind: ExprKind::GetElementRefToLocalVariableArray(
                        lv.clone(),
                        Box::new(index_expr),
                    ),
                };
                Ok(res)
            } else {
                todo!("Implement putting array into local temporary variable")
            }
        } else if let SLPType::AutoderefPointer(internal_ty) = &indexable_expr.ret_type {
            if let SLPType::FixedArray {
                size,
                index_offset,
                ty,
            } = internal_ty.as_ref()
            {
                let index_expr =
                    self.visit_indexation_expression(index, *index_offset, *size, scope)?;
                let res = STExpr {
                    ret_type: SLPType::AutoderefPointer(ty.clone()),
                    loc,
                    kind: ExprKind::GetElementRefToReffedArray(
                        Box::new(indexable_expr),
                        Box::new(index_expr),
                    ),
                };
                Ok(res)
            } else {
                todo!()
            }
        } else {
            todo!()
        }
    }
    fn visit_rhs_expression(
        &mut self,
        expr: &Expr,
        scope: &Scope,
    ) -> Result<RhsExpr, SemTreeBuildErrors> {
        match expr {
            Expr::Ident(l, i) => {
                let id = Id(i.name.clone());
                if let Some((lv, ty)) = scope.get_variable(&id) {
                    Ok(RhsExpr {
                        required_type: ty,
                        loc: *l,
                        kind: RhsKind::LocalVariable(lv),
                    })
                } else {
                    todo!()
                }
            }
            Expr::OpBinIndex(l, i, index) => {
                let visit_array_index = self.visit_array_index(&i, &index, scope, l.clone())?;
                Ok(RhsExpr {
                    required_type: visit_array_index
                        .ret_type
                        .get_underlying_pointer_type()
                        .unwrap()
                        .clone(),
                    loc: l.clone(),
                    kind: RhsKind::Defer(visit_array_index),
                })
            }
            Expr::OpUnDeref(_, _) => todo!(),
            Expr::Constant(_, _) => todo!(),
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
            Expr::OpUnGetRef(_, _) => todo!(),
            Expr::OpFunctionCall(_, _) => todo!(),
            Expr::OpUnAs(_, _, _) => todo!(),
            Expr::OpMethodCall(_, _, _) => todo!(),
            Expr::OpNew(_, _, _) => todo!(),
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

                ast::Constant::Bool(b) => STExpr {
                    ret_type: SLPType::PrimitiveType(crate::types::SLPPrimitiveType::Bool),
                    loc: l.clone(),
                    kind: ExprKind::BoolLiteral(*b),
                },
                ast::Constant::Int(c) => self.visit_int_constant(c, l)?,
                ast::Constant::Float(_) => todo!(),
                ast::Constant::Char(c) => self.visit_char_const(c, l)?,
            },
            Expr::Ident(l, i) => {
                let id = Id(i.name.clone());
                if let Some((lv, ty)) = scope.get_variable(&id) {
                    STExpr {
                        ret_type: ty,
                        loc: *l,
                        kind: ExprKind::LocalVariable(lv),
                    }
                } else {
                    todo!()
                }
            }
            Expr::OpBinPlus(l, x, y) => self.visit_int_bin_op(*l, &x, &y, scope, IntBinOp::Add)?,
            Expr::OpBinMinus(l, x, y) => {
                self.visit_int_bin_op(*l, &x, &y, scope, IntBinOp::Substract)?
            }
            Expr::OpBinAsterisk(l, x, y) => {
                self.visit_int_bin_op(*l, &x, &y, scope, IntBinOp::Multiplication)?
            }
            Expr::OpBinSlash(l, x, y) => {
                self.visit_int_bin_op(*l, &x, &y, scope, IntBinOp::Division)?
            }
            Expr::OpBinDiv(_, _, _) => todo!(),
            Expr::OpBinMod(l, x, y) => {
                self.visit_int_bin_op(*l, &x, &y, scope, IntBinOp::Modulo)?
            }
            Expr::OpUnMinus(l, x) => self.visit_int_unary_op(*l, &x, scope, IntUnaryOp::Minus)?,
            Expr::OpUnNot(l, x) => self.visit_int_unary_op(*l, &x, scope, IntUnaryOp::Inverse)?,

            Expr::OpUnPlus(_, _) => todo!(),
            Expr::OpBinAnd(l, x, y) => self.visit_int_bin_op(*l, &x, &y, scope, IntBinOp::And)?,
            Expr::OpBinOr(l, x, y) => self.visit_int_bin_op(*l, &x, &y, scope, IntBinOp::Or)?,
            Expr::OpBinXor(l, x, y) => self.visit_int_bin_op(*l, &x, &y, scope, IntBinOp::Xor)?,
            Expr::OpBinShl(l, x, y) => self.visit_int_bin_op(*l, &x, &y, scope, IntBinOp::Shl)?,
            Expr::OpBinShr(l, x, y) => self.visit_int_bin_op(*l, &x, &y, scope, IntBinOp::Shr)?,
            Expr::OpBinLesser(l, x, y) => {
                self.visit_int_comparations(*l, &x, &y, scope, ComparationKind::LesserThan)?
            }
            Expr::OpBinGreater(l, x, y) => {
                self.visit_int_comparations(*l, &x, &y, scope, ComparationKind::GreaterThan)?
            }
            Expr::OpBinLesserEq(l, x, y) => {
                self.visit_int_comparations(*l, &x, &y, scope, ComparationKind::LesserEqual)?
            }
            Expr::OpBinGreaterEq(l, x, y) => {
                self.visit_int_comparations(*l, &x, &y, scope, ComparationKind::GreaterEqual)?
            }
            Expr::OpBinEq(l, x, y) => {
                self.visit_int_comparations(*l, &x, &y, scope, ComparationKind::Equal)?
            }
            Expr::OpBinNotEq(l, x, y) => {
                self.visit_int_comparations(*l, &x, &y, scope, ComparationKind::NotEqual)?
            }
            Expr::OpUnDeref(_, _) => todo!(),
            Expr::OpUnGetRef(_, _) => todo!(),
            Expr::OpBinIndex(l, indexable, index) => {
                self.visit_array_index(indexable, index, scope, l.clone())?
            }
            Expr::OpFunctionCall(l, func) => match self.check_kind_of_function(*l, func, scope)? {
                FunctionCallResolveResult::FunctionCall(f) => STExpr {
                    ret_type: f.ret_type.clone(),
                    loc: l.clone(),
                    kind: ExprKind::FunctionCall(f),
                },
                FunctionCallResolveResult::TypeCast(expr) => expr,
            },
            Expr::OpUnAs(l, e, td) => {
                let expr = self.visit_expression(&e, scope)?;
                self.resolve_typecast(&td.ty, expr, l.clone())?
            }
            Expr::OpMethodCall(_, _, _) => todo!(),
            Expr::OpNew(_, _, _) => todo!(),
        })
    }
    fn visit_int_bin_op(
        &mut self,
        loc: Loc,
        l: &Expr,
        r: &Expr,
        scope: &Scope,
        kind: IntBinOp,
    ) -> Result<STExpr, SemTreeBuildErrors> {
        let le = Box::new(Self::insert_autoderef_or_pass(
            self.visit_expression(&l, scope)?,
        )?);
        let re = Box::new(Self::insert_autoderef_or_pass(
            self.visit_expression(&r, scope)?,
        )?);
        if le.ret_type == re.ret_type {
            if le.ret_type.is_any_int() {
                Ok(STExpr {
                    ret_type: le.ret_type.clone(),
                    loc,
                    kind: ExprKind::PrimitiveIntBinOp(le, re, kind),
                })
            } else if le.ret_type.is_bool() {
                let bool_op = match kind {
                    IntBinOp::Or => BoolBinOp::Or,
                    IntBinOp::And => BoolBinOp::And,
                    IntBinOp::Xor => BoolBinOp::Xor,

                    _ => todo!("Proper error handling"),
                };
                Ok(STExpr {
                    ret_type: SLPType::PrimitiveType(SLPPrimitiveType::Bool),
                    loc,
                    kind: ExprKind::BoolBinOp(le, re, bool_op),
                })
            } else {
                todo!("Type error!");
            }
        } else {
            todo!(
                "Non-equal type conversion hierarchy from {:?} to {:?}",
                le.ret_type,
                re.ret_type
            )
        }
    }
    fn visit_int_comparations(
        &mut self,
        loc: Loc,
        l: &Expr,
        r: &Expr,
        scope: &Scope,
        kind: ComparationKind,
    ) -> Result<STExpr, SemTreeBuildErrors> {
        let le = Box::new(Self::insert_autoderef_or_pass(
            self.visit_expression(&l, scope)?,
        )?);
        let re = Box::new(Self::insert_autoderef_or_pass(
            self.visit_expression(&r, scope)?,
        )?);
        if le.ret_type == re.ret_type {
            if le.ret_type.is_any_int() {
                Ok(STExpr {
                    ret_type: SLPType::PrimitiveType(SLPPrimitiveType::Bool),
                    loc,
                    kind: ExprKind::PrimitiveIntComparation(le, re, kind),
                })
            } else if le.ret_type.is_bool() {
                let bool_op = match kind {
                    ComparationKind::Equal => BoolBinOp::Equal,
                    ComparationKind::NotEqual => BoolBinOp::NotEqual,
                    _ => todo!("Proper error handling"),
                };
                Ok(STExpr {
                    ret_type: SLPType::PrimitiveType(SLPPrimitiveType::Bool),
                    loc,
                    kind: ExprKind::BoolBinOp(le, re, bool_op),
                })
            } else {
                todo!("Type error!");
            }
        } else {
            todo!(
                "Non-equal type conversion hierarchy from {:?} to {:?}",
                le.ret_type,
                re.ret_type
            )
        }
    }
    fn visit_int_unary_op(
        &mut self,
        loc: Loc,
        i: &Expr,
        scope: &Scope,
        kind: IntUnaryOp,
    ) -> Result<STExpr, SemTreeBuildErrors> {
        let inp = Box::new(self.visit_expression(&i, scope)?);
        if inp.ret_type.is_any_int() {
            Ok(STExpr {
                ret_type: SLPType::PrimitiveType(SLPPrimitiveType::Bool),
                loc,
                kind: ExprKind::PrimitiveIntUnaryOp(inp, kind),
            })
        } else if inp.ret_type.is_bool() {
            let bool_op = match kind {
                IntUnaryOp::Inverse => BoolUnaryOp::Not,

                _ => todo!("Proper error handling"),
            };
            Ok(STExpr {
                ret_type: SLPType::PrimitiveType(SLPPrimitiveType::Bool),
                loc,
                kind: ExprKind::BoolUnaryOp(inp, bool_op),
            })
        } else {
            todo!("Type error")
        }
    }
}

#[derive(Debug, Clone)]
struct Scope<'a> {
    occupied_global: Rc<RefCell<HashSet<LocalVariable>>>,
    temporary_variables: Rc<RefCell<HashMap<LocalVariable, SLPType>>>,
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
            occupied_global: Default::default(),
            temporary_variables: Default::default(),
        }
    }
    pub fn new_with_outer<'b: 'a>(outer: &'b Scope<'b>) -> Self {
        Scope {
            occupied_global: outer.occupied_global.clone(),
            outer_scope: Some(outer),
            local_variables: Default::default(),
            order: Default::default(),
            temporary_variables: outer.temporary_variables.clone(),
        }
    }
    //fn rec_occupied(&self, lv: &LocalVariable) -> bool {
    //    self.order.contains(lv)
    //        || self
    //            .outer_scope
    //            .map(|x| x.rec_occupied(lv))
    //            .unwrap_or(false)
    //}
    //Перекрывающиеся области видимости
    pub fn add_variable(&mut self, tree_id: &Id, ty: SLPType) -> LocalVariable {
        //Rename all variables in scope of function
        let candidate_name = LocalVariable(tree_id.0.clone());
        let mut testing_name = candidate_name.clone();
        let mut counter = 0;
        //while self.rec_occupied(&testing_name) {
        while self
            .occupied_global
            .as_ref()
            .borrow()
            .contains(&testing_name)
        {
            testing_name = LocalVariable(format!("{}_{}", candidate_name.0, counter));
            counter += 1;
        }
        self.order.push(testing_name.clone());
        self.occupied_global
            .borrow_mut()
            .insert(testing_name.clone());
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
    pub temporary_variables: HashMap<LocalVariable, SLPType>,
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
    Assignment(Loc, Box<RhsExpr>, Box<STExpr>),
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
    TypeCast(STExpr),
}
#[derive(Debug, Clone)]
pub struct RhsExpr {
    pub required_type: SLPType,
    pub loc: Loc,
    pub kind: RhsKind,
}
#[derive(Debug, Clone)]
pub enum RhsKind {
    LocalVariable(LocalVariable),
    Defer(STExpr),
    //Assumes only zero-indexed arrays
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
    TypeCast(Box<STExpr>, TypeConversionKind),
    NumberLiteral(NumberLiteral),
    FloatLiteral(FloatLiteral),
    BoolLiteral(bool),
    CharLiteral(char),
    FunctionCall(FunctionCall),
    PrimitiveIntBinOp(Box<STExpr>, Box<STExpr>, IntBinOp),
    PrimitiveIntUnaryOp(Box<STExpr>, IntUnaryOp),
    PrimitiveIntComparation(Box<STExpr>, Box<STExpr>, ComparationKind),
    BoolBinOp(Box<STExpr>, Box<STExpr>, BoolBinOp),
    BoolUnaryOp(Box<STExpr>, BoolUnaryOp),
    GetElementRefToLocalVariableArray(LocalVariable, Box<STExpr>),
    GetElementRefToReffedArray(Box<STExpr>, Box<STExpr>),
    Deref(Box<STExpr>),
    GetLocalVariableRef(LocalVariable),
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
#[derive(Debug, Clone)]
pub enum FloatLiteral {
    F32(f32),
    F64(f64),
}
impl PartialEq for FloatLiteral {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::F32(l0), Self::F32(r0)) => l0.to_bits() == r0.to_bits(),
            (Self::F64(l0), Self::F64(r0)) => l0.to_bits() == r0.to_bits(),
            _ => false,
        }
    }
}
impl Eq for FloatLiteral {}

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
    Shr,
    Shl,
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
    NotEqual,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BoolUnaryOp {
    Not,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeConversionKind {
    Identity,

    SignedIntExtend,
    UnsignedIntExtend,

    SignedIntTruncate,
    UnsignedIntTruncate,

    SignedToUnsigned,
    UnsignedToSigned,

    SignedToUnsignedExtend,
    UnsignedToSignedExtend,

    SignedToUnsignedTruncate,
    UnsignedToSignedTruncate,

    IntToFloat,
    UintToFloat,
}
