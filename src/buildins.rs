use std::{collections::HashMap, sync::Arc};

use crate::{ast::Loc, semtree::{CodeBlock, Function}, symbols::{GlobalSymbolResolver, Id}, types::{SLPPrimitiveType, SLPType}};

pub struct BuildInModule {
    pub types_resolver: Arc<GlobalSymbolResolver>,
    pub drops: HashMap<SLPType, Id>,
    pub clones: HashMap<SLPType, Id>,
    pub buildins: HashMap<Id, Function>
}
impl BuildInModule {
    pub fn register_or_get_drop(&mut self, ty: &SLPType) -> Option<Id> {
        if let Some(x) = self.drops.get(ty) {
            Some(x.clone())
        } else {
            if ty.is_trivially_copiable() { 
                None
            } 
            else {
                assert!(ty.get_underlying_autoderef_type().is_none());
                let tyname = ty.normalized_name(); 
                let func = Function { function_name: Id(format!("drop@{}", &tyname.0)), function_args: vec![], return_arg: SLPType::PrimitiveType(SLPPrimitiveType::Void), body: CodeBlock::new(), temporary_variables: Default::default(), loc: Loc::new(0, 0) };
                todo!()
            }
        }
    }
    pub fn register_or_get_clone(&mut self, ty: &SLPType) -> Option<Id> {
        if let Some(x) = self.clones.get(ty) {
            Some(x.clone())
        } else {
            if ty.is_trivially_copiable() { 
                None
            } 
            else {
                todo!()
            }
        }
    }
}