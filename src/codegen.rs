use std::{collections::HashMap, sync::Arc};

use inkwell::{
    builder::Builder, context::Context, module::{Linkage, Module}, targets::TargetMachine, types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, IntType}, values::{AggregateValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue}, AddressSpace, IntPredicate
};

use crate::{
    compiler::FileId, semtree::{
        BoolBinOp, CodeBlock, ComparationKind, ExprKind, Function, IntBinOp, LocalVariable, RhsKind, STExpr, STStatement, SemanticTree, VarDecl
    }, symbols::{FunctionDecl, Id, GlobalSymbolResolver}, types::{SLPPrimitiveType, SLPType}
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
    pub fn new<'b: 'a>(
        ctx: &'b CodegenContext,
        module_name: &str,
        target: Arc<TargetMachine>,
    ) -> Codegen<'a> {
        let module: Module<'a> = ctx.context.create_module(module_name);
        let builder: Builder<'a> = ctx.context.create_builder();
        Codegen {
            ctx,
            module,
            builder,
            target_machine: target,
        }
    }
    pub fn register_structs<'b>(&self, tyr: &GlobalSymbolResolver) {
        for ((fid, id), decl) in &tyr.types {
            match decl {
                crate::symbols::SLPTypeDecl::TypeAlias(_) => (),
                crate::symbols::SLPTypeDecl::StructDecl(_) => {
                    let d =  &tyr.reverse_filename_translation[fid];
                    let canonical_name = format!("{}${}", d, &id.0);
                    self.ctx.context.opaque_struct_type(&canonical_name);
                },
            }
        }
        for ((fid, id), decl) in &tyr.types {
            match decl {
                crate::symbols::SLPTypeDecl::TypeAlias(_) => (),
                crate::symbols::SLPTypeDecl::StructDecl(st) => {
                    let d =  &tyr.reverse_filename_translation[fid];
                    let canonical_name = format!("{}${}", d, &id.0);
                    let body: Vec<_> = st.fields.iter().map(|x|&x.1).map(|x|self.slp_type_to_llvm(&x)).collect();
                    self.ctx.context.get_struct_type(&canonical_name).unwrap().set_body(&body, false);
                },
            }
        }
    }
    pub fn compile_semtree<'b>(&self, semtree: &'b SemanticTree) {
        let syms = self.declare_symbols(semtree);

        for f in &semtree.root.funcs {
            //println!("{:?}", f);
            let id = semtree.types_resolver.canonical_functions(
                &semtree.fileid,
                &f.function_name,
            );
            let t = syms.get(&id).unwrap();
            self.compile_function(&f, t, &syms, &semtree.types_resolver);
        }
    }

    pub fn declare_symbols<'b>(
        &self,
        semtree: &'b SemanticTree,
    ) -> HashMap<Id, FunctionValue<'a>> {
        let mut hm = HashMap::new();
        for dep in &semtree.types_resolver.deps[&semtree.fileid] {
            let t = self.declare_symbol(dep, &semtree.types_resolver, true);
            hm.extend(t);
        }
        let t = self.declare_symbol(&semtree.fileid, &semtree.types_resolver, false);
        hm.extend(t);
        hm
    }
    pub fn declare_symbol<'b>(
        &self,
        fid: &FileId,
        tyr: &GlobalSymbolResolver,
        are_external: bool,
    ) -> Vec<(Id, FunctionValue<'a>)> {
        let mut v = vec![];
        if let Some(t) = &tyr.function_declare_order.get(fid) {
        for ids in t.iter() {
            let s = tyr.functions.get(&(fid.clone(), ids.clone())).unwrap();
            match s {
                FunctionDecl::FunctionDecl { loc: _loc, input, output } => {
                    let ty = self.slp_sem_to_llvm_func(&input, output);
                    let name = tyr.canonical_functions(fid, ids);
                    let func = self.module.add_function(
                        &name.0,
                        ty,
                        Some(if are_external {
                            Linkage::External
                        } else {
                            Linkage::External
                        }),
                    );
                    v.push((name, func));
                }
                FunctionDecl::ExternFunctionDecl { loc: _loc, input, output } => {
                    let ty = self.slp_sem_to_llvm_func(&input, output);
                    let name = tyr.canonical_functions(fid, ids);
                    let func = self.module.add_function(
                        &name.0,
                        ty,
                        Some(inkwell::module::Linkage::External),
                    );
                    v.push((name, func));
                }
            }
        }
    }
        v
    }

    pub fn compile_function<'b>(
        &self,
        f: &'b Function,
        func: &FunctionValue,
        syms: &HashMap<Id, FunctionValue<'a>>,
        tyr: &GlobalSymbolResolver,
    ) {
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
            let res = self.builder.build_alloca(ret_ty, "Result");
            prelude.insert(LocalVariable("Result".to_string()), res);
            Some(res)
        } else {
            None
        };

        let body = self.ctx.context.append_basic_block(*func, "body");
        self.builder.build_unconditional_branch(body);
        self.builder.position_at_end(body);
        self.generate_main_body_of_function(func, f, &prelude, syms, tyr);
        let ret_load = ret.map(|x| {
            self.builder
                .build_load(self.slp_type_to_llvm(&f.return_arg), x, "")
        });
        self.builder
            .build_return(ret_load.as_ref().map(|x| x as &dyn BasicValue));
    }
    //Generate local mutable variables;
    fn generate_variable_prelude<'b>(
        &self,
        func: &FunctionValue,
        f: &'b Function,
    ) -> HashMap<LocalVariable, PointerValue<'a>> {
        let mut hm = HashMap::new();

        let input: Vec<_> = func
            .get_params()
            .into_iter()
            .zip(f.function_args.iter())
            .collect();
        //Allocate stack space for input variables
        for (_val, (id, ty)) in &input {
            let ty = self.slp_type_to_llvm(&ty);
            let stackalloc = self.builder.build_alloca(ty, &id.0);
            hm.insert(LocalVariable(id.0.clone()), stackalloc);
        }
        //Allocate stack space for variables in the program
        let variables = f.body
            .common_statements
            .iter().chain(f.body.defer_statements.iter())
            .map(|x| self.get_variables_list(x).into_iter())
            .flatten();
        for v in variables {
            let ty = self.slp_type_to_llvm(&v.ty);
            let stackalloc = self.builder.build_alloca(ty, &v.id.0);
            hm.insert(LocalVariable(v.id.0.clone()), stackalloc);
        }
        //Generate store code for input variables
        for (val, (id, _ty)) in &input {
            self.builder
                .build_store(hm[&LocalVariable(id.0.clone())], val.clone());
        }
        hm
    }
    fn get_variables_list<'b>(&self, stmt: &'b STStatement) -> Vec<&'b VarDecl> {
        match stmt {
            STStatement::CodeBlock(_, b) => b.common_statements
                .iter().chain(b.defer_statements.iter())
                .map(|x| self.get_variables_list(x))
                .flatten()
                .collect(),
            STStatement::Print(_, _) => vec![],
            STStatement::FunctionCall(_, _) => vec![],
            STStatement::Assignment(_, _, st, _) => st.as_ref().map(|x| self.get_variables_list(&x)).unwrap_or(vec![]),
            STStatement::If(_, _, s1, s2) => self
                .get_variables_list(&s1)
                .into_iter()
                .chain(
                    s2.iter()
                        .map(|x| self.get_variables_list(x).into_iter())
                        .flatten(),
                )
                .collect(),
            STStatement::While(_, _, s) => self.get_variables_list(s),
            STStatement::RepeatUntil(_, _, s) => self.get_variables_list(s),
            STStatement::VarDecl(_l, d) => vec![d],
            STStatement::Empty() => vec![],
            STStatement::DeferHint(_, _) => vec![],
            STStatement::BuildInCall(_, _) => todo!(),
            STStatement::MemoryAlloc(_, _) => todo!(),
            STStatement::MemoryFree(_, _) => todo!(),
        }
    }
    pub fn get_pointer_sized_int(&self) -> IntType<'a> {
        let t = self.target_machine.get_target_data().get_pointer_byte_size(None);
        match t {
            4 => self.ctx.context.i32_type(),
            8 => self.ctx.context.i64_type(),
            a @ _  => unimplemented!("WTH, pointer size expected to be only 32 bit or 64 bit {a}"),
        }
    }
    pub fn slp_type_to_llvm(&self, ty: &SLPType) -> BasicTypeEnum<'a> {
        match ty {
            SLPType::PrimitiveType(p) => self.slp_primitive_type_to_llvm(p),
            SLPType::Pointer(b) => self
                .slp_type_to_llvm(&b)
                .ptr_type(Default::default())
                .into(),
            SLPType::AutoderefPointer(b) => self
                .slp_type_to_llvm(&b)
                .ptr_type(Default::default())
                .into(),

            SLPType::DynArray(b) => {
                let arr_lenght = self.get_pointer_sized_int().into();
                let arr_ty = self.slp_type_to_llvm(&b).into_pointer_type().into();
                self.ctx.context.struct_type(&[arr_lenght, arr_ty], false).into()
            },
            SLPType::FixedArray {
                size,
                index_offset: _index_offset,
                ty,
            } => self
                .slp_type_to_llvm(ty)
                .array_type(size.clone().try_into().unwrap())
                .into(),
            SLPType::Struct(fid, n, _) => self.ctx.context.get_struct_type(&format!("{fid}${}", n.0)).unwrap().into(),
            SLPType::RefCounter(b) => {
                let rc_counter = self.get_pointer_sized_int().as_basic_type_enum().into_pointer_type().into();
                let rc_ty = self.slp_type_to_llvm(&b).ptr_type(AddressSpace::default()).into();
                self.ctx.context.struct_type(&[rc_counter, rc_ty], false).into()
            },
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
            SLPPrimitiveType::Float64 => self.ctx.context.f64_type().into(),
            SLPPrimitiveType::Char => self.ctx.context.i32_type().into(),
            SLPPrimitiveType::StringLiteral(_) => todo!(),
        }
    }
    fn build_default<'b>(&self, ty: &'b SLPType, tyr: &GlobalSymbolResolver) -> BasicValueEnum<'a> {
        match ty {
            SLPType::PrimitiveType(p) => match p {
                SLPPrimitiveType::Int8 => self.ctx.context.i8_type().const_zero().into(),
                SLPPrimitiveType::Int16 => self.ctx.context.i16_type().const_zero().into(),
                SLPPrimitiveType::Int32 => self.ctx.context.i32_type().const_zero().into(),
                SLPPrimitiveType::Int64 => self.ctx.context.i64_type().const_zero().into(),
                SLPPrimitiveType::Uint8 => self.ctx.context.i8_type().const_zero().into(),
                SLPPrimitiveType::Uint16 => self.ctx.context.i16_type().const_zero().into(),
                SLPPrimitiveType::Uint32 => self.ctx.context.i32_type().const_zero().into(),
                SLPPrimitiveType::Uint64 => self.ctx.context.i64_type().const_zero().into(),
                SLPPrimitiveType::ISize => self
                .ctx
                .context
                .ptr_sized_int_type(&self.target_machine.get_target_data(), None).const_zero()
                .into(),
                SLPPrimitiveType::USize => self
                .ctx
                .context
                .ptr_sized_int_type(&self.target_machine.get_target_data(), None).const_zero()
                .into(),
                SLPPrimitiveType::Float32 => self.ctx.context.f32_type().const_zero().into(),
                SLPPrimitiveType::Float64 => self.ctx.context.f64_type().const_zero().into(),
                SLPPrimitiveType::String => todo!(),
                SLPPrimitiveType::StringLiteral(_) => todo!(),
                SLPPrimitiveType::Char => self.ctx.context.i32_type().const_zero().into(),
                SLPPrimitiveType::Bool => self.ctx.context.bool_type().const_zero().into(),
                SLPPrimitiveType::Void => todo!(),
            },
            SLPType::Pointer(ptr) => self.slp_type_to_llvm(&ptr).ptr_type(Default::default()).const_null().into(),
            SLPType::AutoderefPointer(_) => unreachable!("Autoderef pointers should never created as default values {ty:#?}"),
            SLPType::DynArray(ty) => {
                let arr_lenght = self.get_pointer_sized_int().into();
                let arr_ty = self.slp_type_to_llvm(&ty).into_pointer_type().into();
                self.ctx.context.struct_type(&[arr_lenght, arr_ty], false).const_zero().into()
            },
            SLPType::FixedArray { size, index_offset, ty } => {
                let val = self.build_default(ty, tyr);
                let llvm_ty = self.slp_type_to_llvm(&ty);
                let mut arr_val = llvm_ty.array_type(size.clone().try_into().unwrap()).const_zero();
                for i in 0..*size {
                    arr_val = self.builder.build_insert_value(arr_val.as_aggregate_value_enum(), val, i.try_into().unwrap(), "default_array").unwrap().into_array_value()

                }
                arr_val.into()
            },
            SLPType::Struct(fid, id, d) => {
                let t = tyr.get_struct(fid, id).unwrap().unwrap();
                let def : Vec<_> = t.fields.iter().map(|x|self.build_default(&x.1, tyr)).collect(); 
                let str = self.slp_type_to_llvm(ty).into_struct_type();
                str.const_named_struct(&def).into()

            },
            SLPType::RefCounter(rc) => self.slp_type_to_llvm(ty).const_zero(),
        }
    }
    fn build_is_null<'b>(&self, expr: &'b STExpr, localvar_stackalloc: &HashMap<LocalVariable, PointerValue<'a>>, syms: &HashMap<Id, FunctionValue<'a>>, tyr: &GlobalSymbolResolver) -> BasicValueEnum<'a> {
        let t = self.visit_expression(expr, localvar_stackalloc, syms, tyr);

        match &expr.ret_type {
            SLPType::PrimitiveType(_) => todo!(),
            SLPType::Pointer(ptr) => self.builder.build_is_null(t.into_pointer_value(), "is_null").into(),
            SLPType::AutoderefPointer(_) => todo!(),
            SLPType::DynArray(_) => {
                //let t: PointerValue<'a> = unsafe { self.builder.build_gep(, t.into_pointer_value(), 0, "rc_nullcheck_1") };
                //let q = self.builder.build_struct_gep(self.get_pointer_sized_int().as_basic_type_enum(), t.into_pointer_value(), 0, "").unwrap();
                let stct = t.into_struct_value();
                let q = self.builder.build_extract_value(stct, 1, "is_null").unwrap();
                self.builder.build_is_null(q.into_pointer_value(), "is_null").into()
            },
            SLPType::FixedArray { size, index_offset, ty } => todo!(),
            SLPType::Struct(_, _, _) => todo!(),
            SLPType::RefCounter(_) => {
                //let t: PointerValue<'a> = unsafe { self.builder.build_gep(, t.into_pointer_value(), 0, "rc_nullcheck_1") };
                //let q = self.builder.build_struct_gep(self.get_pointer_sized_int().as_basic_type_enum(), t.into_pointer_value(), 0, "").unwrap();
                let stct = t.into_struct_value();
                let q = self.builder.build_extract_value(stct, 0, "is_null").unwrap();
                self.builder.build_is_null(q.into_pointer_value(), "is_null").into()
            },
        }
    }
    fn generate_main_body_of_function<'b>(
        &self,
        func: &FunctionValue,
        f: &'b Function,
        localvar_stackalloc: &HashMap<LocalVariable, PointerValue<'a>>,
        syms: &HashMap<Id, FunctionValue<'a>>,
        tyr: &GlobalSymbolResolver,
    ) {
        self.visit_codeblock(&f.body, func, localvar_stackalloc, syms, tyr)
    }
    fn visit_codeblock<'b>(&self,
        cb: &'b CodeBlock,
        func: &FunctionValue,
        localvar_stackalloc: &HashMap<LocalVariable, PointerValue<'a>>,
        syms: &HashMap<Id, FunctionValue<'a>>,
        tyr: &GlobalSymbolResolver,
    ) {
        for i in &cb.common_statements {
            self.visit_statement(i, func, localvar_stackalloc, syms, tyr)
        }
        for j in cb.defer_statements.iter().rev() {
            self.visit_statement(j, func, localvar_stackalloc, syms, tyr)

        }
    }
    fn visit_statement<'b>(
        &self,
        stmt: &'b STStatement,
        func: &FunctionValue,
        localvar_stackalloc: &HashMap<LocalVariable, PointerValue<'a>>,
        syms: &HashMap<Id, FunctionValue<'a>>,
        tyr: &GlobalSymbolResolver,
    ) {
        match stmt {
            STStatement::CodeBlock(_l, stmts) => {
                self.visit_codeblock(stmts, func, localvar_stackalloc, syms, tyr)
            }
            STStatement::Print(_, _) => todo!(),
            STStatement::FunctionCall(_, fc) => {
                let mut vls = vec![];
                for arg in &fc.args {
                    vls.push(self.visit_expression(arg, localvar_stackalloc, syms, tyr))
                }
                let fnct = syms[&fc.func];
                let vls2: Vec<_> = vls
                    .into_iter()
                    .map(|x| -> BasicMetadataValueEnum<'a> { x.into() })
                    .collect();
                self.builder.build_call(fnct, &vls2, "");
            }
            STStatement::Assignment(_l, target, drop, to) => {
                let expr = self.visit_expression(&to, localvar_stackalloc, syms, tyr);
                if let Some(d) = drop {
                    todo!()
                }
                match &target.as_ref().kind {

                    RhsKind::Deref(ptr_expr) => {
                        let ptr = self.visit_expression(ptr_expr, localvar_stackalloc, syms, tyr);
                        let ptr_val = ptr.into_pointer_value();
                        self.builder.build_store(ptr_val, expr);
                    }
                }
            }
            STStatement::If(_loc, cond, mb, ab) => {
                let e = self.visit_expression(&cond, localvar_stackalloc, syms, tyr);
                let int = e.into_int_value();

                if let Some(alt_b) = ab {
                    let branch_main = self.ctx.context.append_basic_block(*func, "branch_main");
                    let branch_alt = self.ctx.context.append_basic_block(*func, "branch_alt");
                    let branch_exit = self.ctx.context.append_basic_block(*func, "branch_exit");

                    self.builder
                        .build_conditional_branch(int, branch_main, branch_alt);
                    self.builder.position_at_end(branch_main);
                    self.visit_statement(&mb, func, localvar_stackalloc, syms, tyr);
                    self.builder.build_unconditional_branch(branch_exit);
                    self.builder.position_at_end(branch_alt);
                    self.visit_statement(&alt_b, func, localvar_stackalloc, syms, tyr);
                    self.builder.build_unconditional_branch(branch_exit);
                    self.builder.position_at_end(branch_exit);
                    let end = func.get_last_basic_block().unwrap();
                    if end != branch_exit {
                        branch_exit.move_after(end).unwrap()
                    }
                } else {
                    let branch_main = self.ctx.context.append_basic_block(*func, "branch_main");
                    let branch_exit = self.ctx.context.append_basic_block(*func, "branch_exit");

                    self.builder
                        .build_conditional_branch(int, branch_main, branch_exit);
                    self.builder.position_at_end(branch_main);
                    self.visit_statement(&mb, func, localvar_stackalloc, syms, tyr);
                    self.builder.build_unconditional_branch(branch_exit);
                    self.builder.position_at_end(branch_exit);
                    let end = func.get_last_basic_block().unwrap();
                    if end != branch_exit {
                        branch_exit.move_after(end).unwrap()
                    }
                }
            }
            STStatement::While(_, cond, body) => {
                let while_cond = self.ctx.context.append_basic_block(*func, "while_cond");
                self.builder.build_unconditional_branch(while_cond);
                self.builder.position_at_end(while_cond);
                let e = self.visit_expression(&cond, localvar_stackalloc, syms, tyr);
                let int = e.into_int_value();
                let while_body = self.ctx.context.append_basic_block(*func, "while_body");
                let while_exit = self.ctx.context.append_basic_block(*func, "while_exit");
                self.builder
                    .build_conditional_branch(int, while_body, while_exit);
                self.builder.position_at_end(while_body);
                self.visit_statement(&body, func, localvar_stackalloc, syms, tyr);
                self.builder.build_unconditional_branch(while_cond);
                self.builder.position_at_end(while_exit);
                let end = func.get_last_basic_block().unwrap();
                if end != while_exit {
                    while_exit.move_after(end).unwrap()
                }
            }
            STStatement::RepeatUntil(_, _, _) => todo!(),
            STStatement::VarDecl(_l, vd) => {
                let expr = self.visit_expression(&vd.init_expr, localvar_stackalloc, syms, tyr);
                self.builder.build_store(localvar_stackalloc[&vd.id], expr);
            }
            STStatement::Empty() => (),
            STStatement::DeferHint(_, _) => (),
            STStatement::BuildInCall(_, _) => todo!(),
            STStatement::MemoryAlloc(_, _) => todo!(),
            STStatement::MemoryFree(_, _) => todo!(),
        }
    }

    fn visit_expression<'b>(
        &self,
        expr: &'b STExpr,
        localvar_stackalloc: &HashMap<LocalVariable, PointerValue<'a>>,
        syms: &HashMap<Id, FunctionValue<'a>>,
        tyr: &GlobalSymbolResolver,
    ) -> BasicValueEnum<'a> {
        let ty = self.slp_type_to_llvm(&expr.ret_type);

        match &expr.kind {
            ExprKind::LocalVariable(lv) => {
                let ptr = localvar_stackalloc[lv].clone();
                let load = self.builder.build_load(ty, ptr, "");
                load
            }
            ExprKind::TypeCast(expr, kind) => {
                let inp = self.visit_expression(expr, localvar_stackalloc, syms, tyr);
                let _source = self.slp_type_to_llvm(&expr.ret_type);
                match kind {
                    crate::semtree::TypeConversionKind::Identity => inp,
                    crate::semtree::TypeConversionKind::SignedIntExtend => self
                        .builder
                        .build_int_cast_sign_flag(
                            inp.into_int_value(),
                            ty.into_int_type(),
                            true,
                            "SignedIntExtend",
                        )
                        .into(),
                    crate::semtree::TypeConversionKind::UnsignedIntExtend => self
                        .builder
                        .build_int_cast_sign_flag(
                            inp.into_int_value(),
                            ty.into_int_type(),
                            false,
                            "UnsignedIntExtend",
                        )
                        .into(),
                    crate::semtree::TypeConversionKind::SignedIntTruncate => self
                        .builder
                        .build_int_cast_sign_flag(
                            inp.into_int_value(),
                            ty.into_int_type(),
                            true,
                            "SignedIntTruncate",
                        )
                        .into(),
                    crate::semtree::TypeConversionKind::UnsignedIntTruncate => self
                        .builder
                        .build_int_cast_sign_flag(
                            inp.into_int_value(),
                            ty.into_int_type(),
                            false,
                            "UnsignedIntTruncate",
                        )
                        .into(),
                    crate::semtree::TypeConversionKind::SignedToUnsigned => inp,
                    crate::semtree::TypeConversionKind::UnsignedToSigned => inp,
                    crate::semtree::TypeConversionKind::SignedToUnsignedExtend => todo!(),
                    crate::semtree::TypeConversionKind::UnsignedToSignedExtend => todo!(),
                    crate::semtree::TypeConversionKind::SignedToUnsignedTruncate => todo!(),
                    crate::semtree::TypeConversionKind::UnsignedToSignedTruncate => todo!(),
                    crate::semtree::TypeConversionKind::IntToFloat => todo!(),
                    crate::semtree::TypeConversionKind::UintToFloat => todo!(),

                }
            }
            ExprKind::NumberLiteral(l) => match l {
                crate::semtree::NumberLiteral::I64(i) => {
                    BasicValueEnum::IntValue(self.ctx.context.i64_type().const_int(*i as u64, true))
                }
                crate::semtree::NumberLiteral::I32(i) => BasicValueEnum::IntValue(
                    self.ctx
                        .context
                        .i32_type()
                        .const_int(*i as i64 as u64, true),
                ),
                crate::semtree::NumberLiteral::I16(i) => BasicValueEnum::IntValue(
                    self.ctx
                        .context
                        .i16_type()
                        .const_int(*i as i64 as u64, true),
                ),
                crate::semtree::NumberLiteral::I8(i) => BasicValueEnum::IntValue(
                    self.ctx.context.i8_type().const_int(*i as i64 as u64, true),
                ),
                crate::semtree::NumberLiteral::U64(i) => BasicValueEnum::IntValue(
                    self.ctx.context.i64_type().const_int(*i as u64, false),
                ),
                crate::semtree::NumberLiteral::U32(i) => BasicValueEnum::IntValue(
                    self.ctx.context.i32_type().const_int(*i as u64, false),
                ),
                crate::semtree::NumberLiteral::U16(i) => BasicValueEnum::IntValue(
                    self.ctx.context.i16_type().const_int(*i as u64, false),
                ),
                crate::semtree::NumberLiteral::U8(i) => {
                    BasicValueEnum::IntValue(self.ctx.context.i8_type().const_int(*i as u64, false))
                }
                crate::semtree::NumberLiteral::ISize(i) => {
                    self.get_pointer_sized_int().const_int(*i as u64, true).into()
                },
            },
            ExprKind::FloatLiteral(_) => todo!(),
            ExprKind::CharLiteral(c) => BasicValueEnum::IntValue(
                self.ctx
                    .context
                    .i32_type()
                    .const_int(c.clone() as u32 as u64, false),
            ),
            ExprKind::FunctionCall(fc) => {
                let mut vls = vec![];
                for arg in &fc.args {
                    vls.push(self.visit_expression(arg, localvar_stackalloc, syms, tyr))
                }
                let fnct = syms[&fc.func];
                let vls2: Vec<_> = vls
                    .into_iter()
                    .map(|x| -> BasicMetadataValueEnum<'a> { x.into() })
                    .collect();
                let csr = self.builder.build_call(fnct, &vls2, "");
                csr.try_as_basic_value().left().unwrap()
            }
            ExprKind::BoolLiteral(b) => inkwell::values::BasicValueEnum::IntValue(
                self.ctx
                    .context
                    .bool_type()
                    .const_int(if *b { 1 } else { 0 }, false),
            ),
            ExprKind::PrimitiveIntBinOp(l, r, k) => {
                let lhs = self.visit_expression(l, localvar_stackalloc, syms, tyr);
                let rhs = self.visit_expression(r, localvar_stackalloc, syms, tyr);
                if !lhs.is_int_value() && !rhs.is_int_value() {
                    panic!("Wrong type!!!");
                }
                let lhs = lhs.into_int_value();
                let rhs = rhs.into_int_value();
                match k {
                    IntBinOp::Add => self.builder.build_int_add(lhs, rhs, "").into(),
                    IntBinOp::Substract => self.builder.build_int_sub(lhs, rhs, "").into(),
                    IntBinOp::Multiplication => self.builder.build_int_mul(lhs, rhs, "").into(),
                    IntBinOp::Division => {
                        if expr.ret_type.is_unsigned_int() {
                            self.builder.build_int_unsigned_div(lhs, rhs, "").into()
                        } else {
                            self.builder.build_int_signed_div(lhs, rhs, "").into()
                        }
                    }
                    IntBinOp::Modulo => {
                        if expr.ret_type.is_unsigned_int() {
                            self.builder.build_int_unsigned_rem(lhs, rhs, "").into()
                        } else {
                            self.builder.build_int_signed_rem(lhs, rhs, "").into()
                        }
                    }
                    IntBinOp::Or => self.builder.build_or(lhs, rhs, "").into(),
                    IntBinOp::And => self.builder.build_and(lhs, rhs, "").into(),
                    IntBinOp::Xor => self.builder.build_xor(lhs, rhs, "").into(),
                    IntBinOp::Shr => self.builder.build_right_shift(lhs, rhs, false, "").into(),
                    IntBinOp::Shl => self.builder.build_left_shift(lhs, rhs, "").into(),
                }
            }
            ExprKind::PrimitiveIntUnaryOp(_, _) => todo!(),
            ExprKind::PrimitiveIntComparation(l, r, k) => {
                let lhs = self.visit_expression(l, localvar_stackalloc, syms, tyr);
                let rhs = self.visit_expression(r, localvar_stackalloc, syms, tyr);
                if !lhs.is_int_value() && !rhs.is_int_value() {
                    panic!("Wrong type!!!");
                }
                let lhs = lhs.into_int_value();
                let rhs = rhs.into_int_value();
                let unsigned = l.ret_type.is_unsigned_int();
                let predicate = match k {
                    ComparationKind::LesserThan => {
                        if unsigned {
                            IntPredicate::ULT
                        } else {
                            IntPredicate::SLT
                        }
                    }
                    ComparationKind::LesserEqual => {
                        if unsigned {
                            IntPredicate::ULE
                        } else {
                            IntPredicate::SLE
                        }
                    }
                    ComparationKind::GreaterThan => {
                        if unsigned {
                            IntPredicate::UGT
                        } else {
                            IntPredicate::SGT
                        }
                    }
                    ComparationKind::GreaterEqual => {
                        if unsigned {
                            IntPredicate::UGE
                        } else {
                            IntPredicate::SGE
                        }
                    }
                    ComparationKind::Equal => IntPredicate::EQ,
                    ComparationKind::NotEqual => IntPredicate::NE,
                };
                self.builder
                    .build_int_compare(predicate, lhs, rhs, "")
                    .into()
            }
            ExprKind::BoolBinOp(l, r, k) => {
                let lhs = self.visit_expression(l, localvar_stackalloc, syms, tyr);
                let rhs = self.visit_expression(r, localvar_stackalloc, syms, tyr);
                if !lhs.is_int_value() && !rhs.is_int_value() {
                    panic!("Wrong type!!!");
                }
                let lhs = lhs.into_int_value();
                let rhs = rhs.into_int_value();
                match k {
                    BoolBinOp::And => self.builder.build_and(lhs, rhs, "").into(),
                    BoolBinOp::Or => self.builder.build_or(lhs, rhs, "").into(),
                    BoolBinOp::Xor => self.builder.build_xor(lhs, rhs, "").into(),
                    BoolBinOp::Equal => self
                        .builder
                        .build_int_compare(IntPredicate::EQ, lhs, rhs, "")
                        .into(),
                    BoolBinOp::NotEqual => self
                        .builder
                        .build_int_compare(IntPredicate::NE, lhs, rhs, "")
                        .into(),
                }
            }
            ExprKind::BoolUnaryOp(l, k) => {
                let inp = self.visit_expression(l, localvar_stackalloc, syms, tyr);
                if !inp.is_int_value() {
                    panic!("Wrong type!!!");
                }
                let inp = inp.into_int_value();
                match k {
                    crate::semtree::BoolUnaryOp::Not => self.builder.build_not(inp, "").into(),
                }
            }
            ExprKind::Deref(d) => {
                let tmp = self.visit_expression(&d, localvar_stackalloc, syms, tyr);
                let ptr = tmp.into_pointer_value();
                let pointee_type = self.slp_type_to_llvm(&expr.ret_type);
                self.builder.build_load(pointee_type, ptr, "")
            }
            ExprKind::GetElementRefInReffedArray(ref_array, index) => {
                let indexable = self.visit_expression(ref_array, localvar_stackalloc, syms, tyr);
                let ptr = indexable.into_pointer_value();
                let index = self.visit_expression(&index, localvar_stackalloc, syms, tyr);
                let pointee_type =
                    self.slp_type_to_llvm(expr.ret_type.get_underlying_autoderef_type().unwrap());
                unsafe {
                    //Pray to compiler gods
                    self.builder
                        .build_gep(pointee_type, ptr, &vec![index.into_int_value()], "")
                        .into()
                }
            }
            ExprKind::GetLocalVariableRef(r) => {
                let ptr = localvar_stackalloc[r].clone();
                ptr.into()
            }
            ExprKind::ConstructRecordFromArgList(args) => {
                let mut t = vec![];
                for i in args {
                    let e = self.visit_expression(i, localvar_stackalloc, syms, tyr);
                    t.push(e);
                }
                let ty = self.slp_type_to_llvm(&expr.ret_type);
                let sty = ty.into_struct_type();
                let mut f = sty.get_undef();
                for (i, val) in t.into_iter().enumerate() {
                    let t = self.builder.build_insert_value(f, val, i as u32, &format!("struct_build_{i}"));
                    let t = t.unwrap();
                    f = t.into_struct_value();
                };
                f.into()
            },
            ExprKind::GetElementRefInReffedRecord(x, y) => {
                let pointee_type =
                    self.slp_type_to_llvm(&x.ret_type.get_underlying_autoderef_type().unwrap());
                let structure = self.visit_expression(x, localvar_stackalloc, syms, tyr);
                let ptr = structure.into_pointer_value();

                self.builder.build_struct_gep(pointee_type, ptr, *y, "").unwrap().into()
            },
            ExprKind::Clone(_) => todo!(),
            ExprKind::BuildInCall(_) => todo!(),
            ExprKind::Default => self.build_default(&expr.ret_type, tyr),
            ExprKind::IsNull(expr) => {
                self.build_is_null(expr, localvar_stackalloc, syms, tyr)
            },
            ExprKind::RefCountDecrease(_) => todo!(),
            ExprKind::RefCountIncrease(_) => todo!(),
            ExprKind::GetElementBehindReffedReferenceCounter(_) => todo!(),
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
    pub fn slp_sem_to_llvm_func(&self, args: &[(Id, SLPType)], ret: &SLPType) -> FunctionType<'a> {
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
