use std::{collections::HashMap, iter, sync::Arc};

use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::TargetMachine,
    types::{
         BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, 
        
    }, values::{AnyValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue},
};

use crate::{
    ast::Loc, semtree::{ExprKind, ExternFunction, Function, LocalVariable, STExpr, STStatement, SemanticTree, VarDecl}, symbols::{ContextSymbolResolver, Id, RawSymbol, RawSymbols}, types::{SLPPrimitiveType, SLPType}
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
            println!("{:?}", f);
            let id = semtree.symbols.main_file_symbols.canonical(&f.function_name, &semtree.symbols.main_file_symbols.decls[&f.function_name]);
            let t = syms.get(&id).unwrap();
            self.compile_function(&f, t, &syms);
        }
    }
    //pub fn declare_symbols<'b>(&self, semtree: &'b SemanticTree) -> HashMap<Id, FunctionValue<'a>> {
    //    let mut syms = HashMap::new();
    //    for ef in &semtree.root.extern_funcs {
    //        let sym = self.declare_extern_function(ef);
    //        syms.insert(ef.function_name.clone(), sym);
    //    }
    //    for f in &semtree.root.main_file_symbols.decl {
    //        let sym = self.declare_impl_function(f);
    //        syms.insert(f.function_name.clone(), sym);
    //    }
    //    syms
    //}
    pub fn declare_symbols<'b>(&self, ctx: &'b ContextSymbolResolver) -> HashMap<Id, FunctionValue<'a>> {
        let t = self.declare_symbol(&ctx.main_file_symbols, false);
        let q: Vec<_> = ctx.deps_symbols.iter().map(|x|self.declare_symbol(&x, true)).flatten().collect();
        t.into_iter().chain(q).collect()
    }
    pub fn declare_symbol<'b>(&self, sym: &'b RawSymbols, are_external: bool) -> Vec<(Id, FunctionValue<'a>)> {
        let mut v = vec![];
        for (id, s) in &sym.decls {
            match s {
                RawSymbol::FunctionDecl { loc, input, output } => {
                    let ty = self.slp_sem_to_llvm_func(&input, output);
                    let name = sym.canonical(id, s);
                    let func = self.module.add_function(
                        &name.0,
                        ty,
                        Some(if are_external {Linkage::External} else {Linkage::External}),
                    );
                    v.push((name, func));
                },
                RawSymbol::ExternFunctionDecl { loc, input, output } => {
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
                self.get_variables_list(&s1).into_iter().chain(s2.iter().map(|x|self.get_variables_list(stmt).into_iter()).flatten()).collect(),
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
        }
    }
    fn generate_main_body_of_function<'b>(&self, func: &FunctionValue, f: &'b Function, localvar_stackalloc: &HashMap<LocalVariable, PointerValue<'a>>, syms: &HashMap<Id, FunctionValue<'a>>) {
        for i in &f.body {
            self.visit_statement(i, localvar_stackalloc, syms)
        }
    }
    fn visit_statement<'b>(&self, stmt: &'b STStatement, localvar_stackalloc: &HashMap<LocalVariable, PointerValue<'a>>, syms: &HashMap<Id, FunctionValue<'a>>) {
        match stmt {
            STStatement::CodeBlock(_, _) => todo!(),
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
            STStatement::If(_, _, _, _) => todo!(),
            STStatement::While(_, _, _) => todo!(),
            STStatement::RepeatUntil(_, _, _) => todo!(),
            STStatement::VarDecl(l, vd) => {
                if let Some(init_expr) = &vd.init_expr {
                    let expr = self.visit_expression(init_expr, localvar_stackalloc, syms);
                    self.builder.build_store(localvar_stackalloc[&vd.id], expr);
                }     
                
            },
            STStatement::Empty() => todo!(),
        }
    }
    fn visit_expression<'b>(&self, expr: &'b STExpr, localvar_stackalloc: &HashMap<LocalVariable, PointerValue<'a>>, syms: &HashMap<Id, FunctionValue<'a>>) -> BasicValueEnum<'a> {
        let ty = self.slp_type_to_llvm(&expr.ret_type);
        match &expr.kind {
            crate::semtree::ExprKind::LocalVariable(lv) => {
                let ptr = localvar_stackalloc[lv].clone();
                let load = self.builder.build_load(ty, ptr, "");
                load
            },
            crate::semtree::ExprKind::TypeCast(_) => todo!(),
            crate::semtree::ExprKind::NumberLiteral(l) => {
                match l {
                    crate::semtree::NumberLiteral::I64(_) => todo!(),
                    crate::semtree::NumberLiteral::I32(i) => BasicValueEnum::IntValue(self.ctx.context.i32_type().const_int(*i as u32 as u64, true)),
                    crate::semtree::NumberLiteral::I16(_) => todo!(),
                    crate::semtree::NumberLiteral::I8(i) => BasicValueEnum::IntValue(self.ctx.context.i8_type().const_int(*i as u8 as u64, true)),
                    crate::semtree::NumberLiteral::U32(_) => todo!(),
                    crate::semtree::NumberLiteral::U64(_) => todo!(),
                }
            },
            crate::semtree::ExprKind::FunctionCall(fc) => {
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
