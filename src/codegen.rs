use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::TargetMachine,
    types::{
        AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, PointerType,
        StructType,
    },
};

use crate::{
    semtree::{ExternFunction, SemanticTree},
    types::{SLPPrimitiveType, SLPType},
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
    pub fn compile_semtree(&self, semtree: &SemanticTree) {
        for ef in &semtree.root.extern_funcs {
            self.compile_extern_function(ef);
        }
    }
    pub fn compile_extern_function(&self, ef: &ExternFunction) {
        let f = self.slp_func_to_llvm_func(&ef.function_args, &ef.return_arg);
        let _func = self.module.add_function(
            &ef.function_name.0,
            f,
            Some(inkwell::module::Linkage::External),
        );
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
