use std::{collections::HashMap, iter};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::TargetMachine,
    types::{
         BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, 
        
    }, values::{AnyValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue},
};

use crate::{
    ast::Loc, semtree::{ExprKind, ExternFunction, Function, LocalVariable, STExpr, STStatement, SemanticTree, VarDecl}, symbols::Id, types::{SLPPrimitiveType, SLPType}
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
    pub target_machine: TargetMachine,
}
impl<'a> Codegen<'a> {
    pub fn new<'b: 'a>(ctx: &'b CodegenContext, module_name: &str, target: TargetMachine) -> Codegen<'a> {
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
        let syms = self.declare_symbols(semtree);
        
        for f in &semtree.root.funcs {
            let t = syms.get(&f.function_name).unwrap();
            self.compile_function(&f, t, &syms);
        }
    }
    pub fn declare_symbols<'b>(&self, semtree: &'b SemanticTree) -> HashMap<Id, FunctionValue<'a>> {
        let mut syms = HashMap::new();
        for ef in &semtree.root.extern_funcs {
            let sym = self.declare_extern_function(ef);
            syms.insert(ef.function_name.clone(), sym);
        }
        for f in &semtree.root.funcs {
            let sym = self.declare_impl_function(f);
            syms.insert(f.function_name.clone(), sym);
        }
        syms
    }
    pub fn declare_extern_function(&self, ef: &ExternFunction) -> FunctionValue<'a> {
        let f = self.slp_func_to_llvm_func(&ef.function_args, &ef.return_arg);
        let func = self.module.add_function(
            &ef.function_name.0,
            f,
            Some(inkwell::module::Linkage::External),
        );
        func
    }
    pub fn declare_impl_function(&self, ef: &Function) -> FunctionValue<'a> {
        let f = self.slp_func_to_llvm_func(&ef.function_args, &ef.return_arg);
        let func = self.module.add_function(
            &ef.function_name.0,
            f,
            Some(inkwell::module::Linkage::External),
        );
        func
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
        
        let variables = f.body.iter().map(|x|self.get_variables_list(x).into_iter()).flatten();
        let mut hm = HashMap::new();
        for v in variables {
            let ty = self.slp_type_to_llvm(&v.ty);
            let stackalloc = self.builder.build_alloca(ty, &v.id.0);
            hm.insert(v.id.clone(), stackalloc);
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
                    vls.push(self.visit_expression(arg, localvar_stackalloc))
                }
                let fnct = syms[&fc.func];
                let vls2: Vec<_> = 
                    vls.into_iter().map(|x|->BasicMetadataValueEnum<'a>{
                        x.into()
                    }).collect();
                self.builder.build_call(fnct, &vls2, "");
            },
            STStatement::Assignment(l, target, to) => {
                let expr = self.visit_expression(&to, localvar_stackalloc);
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
                    let expr = self.visit_expression(init_expr, localvar_stackalloc);
                    self.builder.build_store(localvar_stackalloc[&vd.id], expr);
                }     
                
            },
            STStatement::Empty() => todo!(),
        }
    }
    fn visit_expression<'b>(&self, expr: &'b STExpr, localvar_stackalloc: &HashMap<LocalVariable, PointerValue<'a>>) -> BasicValueEnum<'a> {
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
            crate::semtree::ExprKind::FunctionCall(_) => todo!(),
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
}
