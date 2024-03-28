use std::{collections::HashMap, iter, sync::Arc};

use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::TargetMachine,
    types::{
         BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, 
        
    }, values::{AnyValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, IntMathValue, IntValue, PointerValue}, IntPredicate,
};

use crate::{
    ast::Loc, semtree::{BoolBinOp, ComparationKind, ExprKind, ExternFunction, Function, IntBinOp, LocalVariable, STExpr, STStatement, SemanticTree, VarDecl}, symbols::{ContextSymbolResolver, Id, FunctionDecl, Symbols}, types::{SLPPrimitiveType, SLPType}
};

pub struct CodegenContext {
    pub context: Context,
}
impl CodegenContext {
    pub fn new() -> Self {
        CodegenContext {
            context: Context::create(),
        }
    }
}
pub struct Codegen<'a> {
    pub ctx: &'a CodegenContext,
    pub module: Module<'a>,
    pub builder: Builder<'a>,
    pub target_machine: Arc<TargetMachine>,
}
impl<'a> Codegen<'a> {
    pub fn new<'b: 'a>(ctx: &'b CodegenContext, module_name: &str, target: Arc<TargetMachine>) -> Codegen<'a> {
        let module: Module<'a> = ctx.context.create_module(module_name);
        let builder: Builder<'a> = ctx.context.create_builder();
        Codegen {
            ctx,
            module,
            builder,
            target_machine: target,
        }
    }
    pub fn compile_semtree<'b>(&self, semtree: &'b SemanticTree) {
        let syms = self.declare_symbols(&semtree.symbols);
        
        for f in &semtree.root.funcs {
            //println!("{:?}", f);
            let id = semtree.symbols.main_file_symbols.canonical(&f.function_name, &semtree.symbols.main_file_symbols.func_decls[&f.function_name]);
            let t = syms.get(&id).unwrap();
            self.compile_function(&f, t, &syms);
        }
    }

    pub fn declare_symbols<'b>(&self, ctx: &'b ContextSymbolResolver) -> HashMap<Id, FunctionValue<'a>> {
        let t = self.declare_symbol(&ctx.main_file_symbols, false);
        let q: Vec<_> = ctx.deps_symbols.iter().map(|x|self.declare_symbol(&x, true)).flatten().collect();
        t.into_iter().chain(q).collect()
    }
    pub fn declare_symbol<'b>(&self, sym: &'b Symbols, are_external: bool) -> Vec<(Id, FunctionValue<'a>)> {
        let mut v = vec![];
        for (id, s) in &sym.func_decls {
            match s {
                FunctionDecl::FunctionDecl { loc, input, output } => {
                    let ty = self.slp_sem_to_llvm_func(&input, output);
                    let name = sym.canonical(id, s);
                    let func = self.module.add_function(
                        &name.0,
                        ty,
                        Some(if are_external {Linkage::External} else {Linkage::External}),
                    );
                    v.push((name, func));
                },
                FunctionDecl::ExternFunctionDecl { loc, input, output } => {
                    let ty = self.slp_sem_to_llvm_func(&input, output);
                    let name = sym.canonical(id, s);
                    let func = self.module.add_function(
                        &name.0,
                        ty,
                        Some(inkwell::module::Linkage::External),
                    );
                    v.push((name, func));

                },
            }
        }
        v
    }

    pub fn compile_function<'b>(&self, f: &'b Function, func: &FunctionValue, syms: &HashMap<Id, FunctionValue<'a>>) {
        //let f_t =  self.slp_func_to_llvm_func(&f.function_args, &f.return_arg);
        //let func = self.module.add_function(
        //    &f.function_name.0,
        //    f_t,
        //    Some(inkwell::module::Linkage::External),
        //);
        let t = self.ctx.context.append_basic_block(*func, "entry");
        self.builder.position_at_end(t);
        let mut prelude = self.generate_variable_prelude(&func, f);
        let ret = if !f.return_arg.is_void() {
            let ret_ty = self.slp_type_to_llvm(&f.return_arg);
            let res = self.builder.build_alloca( ret_ty, "Result");
            prelude.insert(LocalVariable("Result".to_string()), res);
            Some(res)
        } else {None};

        let body = self.ctx.context.append_basic_block(*func, "body");
        self.builder.build_unconditional_branch(body);        
        self.builder.position_at_end(body);
        self.generate_main_body_of_function(func, f, &prelude, syms);
        let ret_load = ret.map(|x|self.builder.build_load(self.slp_type_to_llvm(&f.return_arg), x, ""));
        self.builder.build_return(ret_load.as_ref().map(|x|x as &dyn BasicValue));
    }
    //Generate local mutable variables;
    fn generate_variable_prelude<'b>(&self, func: &FunctionValue, f: &'b Function) -> HashMap<LocalVariable, PointerValue<'a>> {
        let mut hm = HashMap::new();

        let input: Vec<_> = func.get_params().into_iter().zip(f.function_args.iter()).collect();
        //Allocate stack space for input variables
        for (val, (id, ty)) in &input {
            let ty = self.slp_type_to_llvm(&ty);
            let stackalloc = self.builder.build_alloca(ty, &id.0);
            hm.insert(LocalVariable(id.0.clone()), stackalloc);
        }
        //Allocate stack space for variables in the program
        let variables = f.body.iter().map(|x|self.get_variables_list(x).into_iter()).flatten();
        for v in variables {
            let ty = self.slp_type_to_llvm(&v.ty);
            let stackalloc = self.builder.build_alloca(ty, &v.id.0);
            hm.insert(LocalVariable(v.id.0.clone()), stackalloc);
        }
        //Generate store code for input variables
        for (val, (id, ty)) in &input {
            self.builder.build_store(hm[&LocalVariable(id.0.clone())], val.clone());
        }
        hm
    }
    fn get_variables_list<'b>(&self, stmt: &'b STStatement) -> Vec<&'b VarDecl> {
        match stmt {
            STStatement::CodeBlock(_, b) => b.iter().map(|x|self.get_variables_list(x)).flatten().collect(),
            STStatement::Print(_, _) => vec![],
            STStatement::FunctionCall(_, _) => vec![],
            STStatement::Assignment(_, _, _) => vec![],
            STStatement::If(_, _, s1, s2) => 
                self.get_variables_list(&s1).into_iter().chain(s2.iter().map(|x|self.get_variables_list(x).into_iter()).flatten()).collect(),
            STStatement::While(_, _, s) => self.get_variables_list(s),
            STStatement::RepeatUntil(_, _, s) => self.get_variables_list(s),
            STStatement::VarDecl(l, d) => vec![d],
            STStatement::Empty() => vec![],
        }
    }
    pub fn slp_type_to_llvm(&self, ty: &SLPType) -> BasicTypeEnum<'a> {
        match ty {
            SLPType::PrimitiveType(p) => self.slp_primitive_type_to_llvm(p),
            SLPType::Pointer(b) => self
                .slp_type_to_llvm(&b)
                .ptr_type(Default::default())
                .into(),
            SLPType::DynArray(_) => todo!(),
            SLPType::FixedArray { begin, end, ty } => todo!(),
            SLPType::Struct(_) => todo!(),
        }
    }
    pub fn slp_primitive_type_to_llvm(&self, ty: &SLPPrimitiveType) -> BasicTypeEnum<'a> {
        match ty {
            SLPPrimitiveType::Int8 => self.ctx.context.i8_type().into(),
            SLPPrimitiveType::Int16 => self.ctx.context.i16_type().into(),
            SLPPrimitiveType::Int32 => self.ctx.context.i32_type().into(),
            SLPPrimitiveType::Int64 => self.ctx.context.i64_type().into(),
            SLPPrimitiveType::Uint8 => self.ctx.context.i8_type().into(),
            SLPPrimitiveType::Uint16 => self.ctx.context.i16_type().into(),
            SLPPrimitiveType::Uint32 => self.ctx.context.i32_type().into(),
            SLPPrimitiveType::Uint64 => self.ctx.context.i64_type().into(),
            SLPPrimitiveType::ISize => self
                .ctx
                .context
                .ptr_sized_int_type(&self.target_machine.get_target_data(), None)
                .into(),
            SLPPrimitiveType::USize => self
                .ctx
                .context
                .ptr_sized_int_type(&self.target_machine.get_target_data(), None)
                .into(),
            SLPPrimitiveType::String => todo!(),
            SLPPrimitiveType::Bool => self.ctx.context.bool_type().into(),
            SLPPrimitiveType::Void => panic!("Void type encountered outside function return value"),
            SLPPrimitiveType::Float32 => self.ctx.context.f32_type().into(),
            SLPPrimitiveType::Float64 => self.ctx.context.f32_type().into(),
        }
    }
    fn generate_main_body_of_function<'b>(&self, func: &FunctionValue, f: &'b Function, localvar_stackalloc: &HashMap<LocalVariable, PointerValue<'a>>, syms: &HashMap<Id, FunctionValue<'a>>) {
        for i in &f.body {
            self.visit_statement(i, func, localvar_stackalloc, syms)
        }
    }
    fn visit_statement<'b>(&self, stmt: &'b STStatement, func: &FunctionValue, localvar_stackalloc: &HashMap<LocalVariable, PointerValue<'a>>, syms: &HashMap<Id, FunctionValue<'a>>) {
        match stmt {
            STStatement::CodeBlock(l, stmts) => {
                //let codeblock_begin = self.ctx.context.append_basic_block(*func, "codeblock_begin");
                //self.builder.build_unconditional_branch(codeblock_begin);

                //let codeblock_end = self.ctx.context.append_basic_block(*func, "codeblock_end");
                //self.builder.position_at_end(codeblock_begin);
                for i in stmts {
                    self.visit_statement(i, func, localvar_stackalloc, syms)
                }
                //self.builder.build_unconditional_branch(codeblock_end);
                //self.builder.position_at_end(codeblock_end);
                
            },
            STStatement::Print(_, _) => todo!(),
            STStatement::FunctionCall(_, fc) => {
                let mut vls = vec![];
                for arg in &fc.args {
                    vls.push(self.visit_expression(arg, localvar_stackalloc, syms))
                }
                let fnct = syms[&fc.func];
                let vls2: Vec<_> = 
                    vls.into_iter().map(|x|->BasicMetadataValueEnum<'a>{
                        x.into()
                    }).collect();
                self.builder.build_call(fnct, &vls2, "");
            },
            STStatement::Assignment(l, target, to) => {
                let expr = self.visit_expression(&to, localvar_stackalloc, syms);
                if let ExprKind::LocalVariable(lv) = &target.kind {
                    let var = localvar_stackalloc[lv];
                    self.builder.build_store(var, expr);
                }
                else {
                    todo!("Expression not supported! Loc: {}", l);
                }
            },
            STStatement::If(loc, cond, mb, ab) => {
                let e = self.visit_expression(&cond, localvar_stackalloc, syms);
                let int = e.into_int_value();
                
                if let Some(alt_b) = ab {
                    let branch_main = self.ctx.context.append_basic_block(*func, "branch_main");
                    let branch_alt = self.ctx.context.append_basic_block(*func, "branch_alt");
                    let branch_exit = self.ctx.context.append_basic_block(*func, "branch_exit");
                    
                    self.builder.build_conditional_branch(int, branch_main, branch_alt);
                    self.builder.position_at_end(branch_main);
                    self.visit_statement(&mb, func, localvar_stackalloc, syms);
                    self.builder.build_unconditional_branch(branch_exit);
                    self.builder.position_at_end(branch_alt);
                    self.visit_statement(&alt_b, func, localvar_stackalloc, syms);
                    self.builder.build_unconditional_branch(branch_exit);
                    self.builder.position_at_end(branch_exit);
                    let end = func.get_last_basic_block().unwrap();
                    if end != branch_exit {
                        branch_exit.move_after(end).unwrap()
                    }
                }
                else {
                    let branch_main = self.ctx.context.append_basic_block(*func, "branch_main");
                    let branch_exit = self.ctx.context.append_basic_block(*func, "branch_exit");
                    
                    self.builder.build_conditional_branch(int, branch_main, branch_exit);
                    self.builder.position_at_end(branch_main);
                    self.visit_statement(&mb, func, localvar_stackalloc, syms);
                    self.builder.build_unconditional_branch(branch_exit);
                    self.builder.position_at_end(branch_exit);
                    let end = func.get_last_basic_block().unwrap();
                    if end != branch_exit {
                        branch_exit.move_after(end).unwrap()
                    }
                }
            },
            STStatement::While(_, cond, body) => {
                
                let while_cond = self.ctx.context.append_basic_block(*func, "while_cond");
                self.builder.build_unconditional_branch(while_cond);
                self.builder.position_at_end(while_cond);
                let e = self.visit_expression(&cond, localvar_stackalloc, syms);
                let int = e.into_int_value();
                let while_body = self.ctx.context.append_basic_block(*func, "while_body");
                let while_exit = self.ctx.context.append_basic_block(*func, "while_exit");
                self.builder.build_conditional_branch(int, while_body, while_exit);
                self.builder.position_at_end(while_body);
                self.visit_statement(&body, func, localvar_stackalloc, syms);
                self.builder.build_unconditional_branch(while_cond);
                self.builder.position_at_end(while_exit);
                let end = func.get_last_basic_block().unwrap();
                if end != while_exit {
                    while_exit.move_after(end).unwrap()
                }
                
            },
            STStatement::RepeatUntil(_, _, _) => todo!(),
            STStatement::VarDecl(l, vd) => {
                if let Some(init_expr) = &vd.init_expr {
                    let expr = self.visit_expression(init_expr, localvar_stackalloc, syms);
                    self.builder.build_store(localvar_stackalloc[&vd.id], expr);
                }     
                
            },
            STStatement::Empty() => (),
        }
    }
    fn visit_expression<'b>(&self, expr: &'b STExpr, localvar_stackalloc: &HashMap<LocalVariable, PointerValue<'a>>, syms: &HashMap<Id, FunctionValue<'a>>) -> BasicValueEnum<'a> {
        let ty = self.slp_type_to_llvm(&expr.ret_type);
        
        match &expr.kind {
            ExprKind::LocalVariable(lv) => {
                let ptr = localvar_stackalloc[lv].clone();
                let load = self.builder.build_load(ty, ptr, "");
                load
            },
            ExprKind::TypeCast(expr, kind) => {
                let inp = self.visit_expression(expr, localvar_stackalloc, syms);
                let source = self.slp_type_to_llvm(&expr.ret_type);
                match kind {
                    crate::semtree::TypeConversionKind::Identity => inp,
                    crate::semtree::TypeConversionKind::SignedIntExtend => {self.builder.build_int_cast_sign_flag(inp.into_int_value(), ty.into_int_type(), true , "SignedIntExtend").into()},
                    crate::semtree::TypeConversionKind::UnsignedIntExtend => {self.builder.build_int_cast_sign_flag(inp.into_int_value(), ty.into_int_type(), false , "UnsignedIntExtend").into()},
                    crate::semtree::TypeConversionKind::SignedIntTruncate => {self.builder.build_int_cast_sign_flag(inp.into_int_value(), ty.into_int_type(), true , "SignedIntTruncate").into()},
                    crate::semtree::TypeConversionKind::UnsignedIntTruncate => {self.builder.build_int_cast_sign_flag(inp.into_int_value(), ty.into_int_type(), false , "UnsignedIntTruncate").into()},
                    crate::semtree::TypeConversionKind::SignedToUnsigned => todo!(),
                    crate::semtree::TypeConversionKind::UnsignedToSigned => todo!(),
                    crate::semtree::TypeConversionKind::SignedToUnsignedExtend => todo!(),
                    crate::semtree::TypeConversionKind::UnsignedToSignedExtend => todo!(),
                    crate::semtree::TypeConversionKind::SignedToUnsignedTruncate => todo!(),
                    crate::semtree::TypeConversionKind::UnsignedToSignedTruncate => todo!(),
                    crate::semtree::TypeConversionKind::IntToFloat => todo!(),
                    crate::semtree::TypeConversionKind::UintToFloat => todo!(),
                }
            }
            ExprKind::NumberLiteral(l) => {
                match l {
                    crate::semtree::NumberLiteral::I64(i) =>  BasicValueEnum::IntValue(self.ctx.context.i64_type().const_int(*i as u64, true)),
                    crate::semtree::NumberLiteral::I32(i) => BasicValueEnum::IntValue(self.ctx.context.i32_type().const_int(*i as i64 as u64, true)),
                    crate::semtree::NumberLiteral::I16(i) => BasicValueEnum::IntValue(self.ctx.context.i16_type().const_int(*i as i64 as u64, true)),
                    crate::semtree::NumberLiteral::I8(i) => BasicValueEnum::IntValue(self.ctx.context.i8_type().const_int(*i as i64 as u64, true)),
                    crate::semtree::NumberLiteral::U64(i) => BasicValueEnum::IntValue(self.ctx.context.i64_type().const_int(*i as u64, false)),
                    crate::semtree::NumberLiteral::U32(i) => BasicValueEnum::IntValue(self.ctx.context.i32_type().const_int(*i as u64, false)),
                    crate::semtree::NumberLiteral::U16(i) => BasicValueEnum::IntValue(self.ctx.context.i16_type().const_int(*i as u64, false)),
                    crate::semtree::NumberLiteral::U8(i) => BasicValueEnum::IntValue(self.ctx.context.i8_type().const_int(*i as u64, false)),
                }
            },
            ExprKind::FunctionCall(fc) => {
                let mut vls = vec![];
                for arg in &fc.args {
                    vls.push(self.visit_expression(arg, localvar_stackalloc, syms))
                }
                let fnct = syms[&fc.func];
                let vls2: Vec<_> = 
                    vls.into_iter().map(|x|->BasicMetadataValueEnum<'a>{
                        x.into()
                    }).collect();
                let csr = self.builder.build_call(fnct, &vls2, "");
                csr.try_as_basic_value().left().unwrap()
            },
            ExprKind::BoolLiteral(b) => inkwell::values::BasicValueEnum::IntValue(self.ctx.context.bool_type().const_int(if *b {1} else {0}, false)),
            ExprKind::PrimitiveIntBinOp(l, r, k) => {
                let lhs = self.visit_expression(l, localvar_stackalloc, syms);
                let rhs = self.visit_expression(r, localvar_stackalloc, syms);
                if !lhs.is_int_value() && !rhs.is_int_value() {
                    panic!("Wrong type!!!");
                }
                let lhs = lhs.into_int_value();
                let rhs = rhs.into_int_value();
                match k {
                    IntBinOp::Add => self.builder.build_int_add(lhs, rhs, "").into(),
                    IntBinOp::Substract => self.builder.build_int_sub(lhs, rhs, "").into(),
                    IntBinOp::Multiplication => self.builder.build_int_mul(lhs, rhs, "").into(),
                    IntBinOp::Division => if expr.ret_type.is_unsigned_int() {
                        self.builder.build_int_unsigned_div(lhs, rhs, "").into()
                    }
                    else {
                        self.builder.build_int_signed_div(lhs, rhs, "").into()
                    }
                    IntBinOp::Modulo => if expr.ret_type.is_unsigned_int() {
                        self.builder.build_int_unsigned_rem(lhs, rhs, "").into()
                    }
                    else {
                        self.builder.build_int_signed_rem(lhs, rhs, "").into()
                    }
                    IntBinOp::Or => self.builder.build_or(lhs, rhs, "").into(),
                    IntBinOp::And => self.builder.build_and(lhs, rhs, "").into(),
                    IntBinOp::Xor => self.builder.build_xor(lhs, rhs, "").into(),
                }
            },
            ExprKind::PrimitiveIntUnaryOp(_, _) => todo!(),
            ExprKind::PrimitiveIntComparation(l, r, k) => {
                let lhs = self.visit_expression(l, localvar_stackalloc, syms);
                let rhs = self.visit_expression(r, localvar_stackalloc, syms);
                if !lhs.is_int_value() && !rhs.is_int_value() {
                    panic!("Wrong type!!!");
                }
                let lhs = lhs.into_int_value();
                let rhs = rhs.into_int_value();
                let unsigned = l.ret_type.is_unsigned_int();
                let predicate = match k {
                    ComparationKind::LesserThan => if unsigned {IntPredicate::ULT} else {IntPredicate::SLT}
                    ComparationKind::LesserEqual => if unsigned {IntPredicate::ULE} else {IntPredicate::SLE},
                    ComparationKind::GreaterThan => if unsigned {IntPredicate::UGT} else {IntPredicate::SGT},
                    ComparationKind::GreaterEqual => if unsigned {IntPredicate::UGE} else {IntPredicate::SGE},
                    ComparationKind::Equal => IntPredicate::EQ,
                    ComparationKind::NotEqual => IntPredicate::NE,
                };
                self.builder.build_int_compare(predicate, lhs, rhs, "").into()
            },
            ExprKind::BoolBinOp(l, r, k) => {
                let lhs = self.visit_expression(l, localvar_stackalloc, syms);
                let rhs = self.visit_expression(r, localvar_stackalloc, syms);
                if !lhs.is_int_value() && !rhs.is_int_value() {
                    panic!("Wrong type!!!");
                }
                let lhs = lhs.into_int_value();
                let rhs = rhs.into_int_value();
                match k {
                    BoolBinOp::And => self.builder.build_and(lhs, rhs, "").into(),
                    BoolBinOp::Or => self.builder.build_or(lhs, rhs, "").into(),
                    BoolBinOp::Xor => self.builder.build_xor(lhs, rhs, "").into(),
                    BoolBinOp::Equal => self.builder.build_int_compare(IntPredicate::EQ, lhs, rhs, "").into(),
                    BoolBinOp::NotEqual => self.builder.build_int_compare(IntPredicate::NE, lhs, rhs, "").into(),
                }
            }
            ExprKind::BoolUnaryOp(l, k) => {
                let inp = self.visit_expression(l, localvar_stackalloc, syms);
                if !inp.is_int_value() {
                    panic!("Wrong type!!!");
                }
                let inp = inp.into_int_value();
                match k {
                    crate::semtree::BoolUnaryOp::Not => self.builder.build_not(inp, "").into(),
                }
            },
            ExprKind::FloatLiteral(_) => todo!(),
        }
                
    }
    pub fn slp_func_to_llvm_func(
        &self,
        args: &[(crate::symbols::Id, SLPType)],
        ret: &SLPType,
    ) -> FunctionType<'a> {
        let args_list: Vec<BasicMetadataTypeEnum<'a>> = args
            .iter()
            .map(|x| self.slp_type_to_llvm(&x.1).into())
            .collect();
        match ret {
            SLPType::PrimitiveType(SLPPrimitiveType::Void) => {
                self.ctx.context.void_type().fn_type(&args_list, false)
            }
            y => self.slp_type_to_llvm(y).fn_type(&args_list, false),
        }
    }
    pub fn slp_sem_to_llvm_func(
        &self,
        args: &[SLPType],
        ret: &SLPType,
    ) -> FunctionType<'a> {
        let args_list: Vec<BasicMetadataTypeEnum<'a>> = args
            .iter()
            .map(|x| self.slp_type_to_llvm(&x).into())
            .collect();
        match ret {
            SLPType::PrimitiveType(SLPPrimitiveType::Void) => {
                self.ctx.context.void_type().fn_type(&args_list, false)
            }
            y => self.slp_type_to_llvm(y).fn_type(&args_list, false),
        }
    }
}
