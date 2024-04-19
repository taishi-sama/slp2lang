use std::{collections::HashMap, sync::Arc};

use crate::{ast::Loc, semtree::{CodeBlock, ExprKind, Function, LocalVariable, RhsExpr, STExpr, STStatement}, symbols::{GlobalSymbolResolver, Id}, types::{SLPPrimitiveType, SLPType}};

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
                let zero_zero_loc = Loc::new(0, 0);
                assert!(ty.get_underlying_autoderef_type().is_none());
                let tyname = ty.normalized_name(); 
                let mut code_block = CodeBlock::new();
                let input_name = "input".to_string();
                if let SLPType::RefCounter(rc) = ty {
                    let local_variable_ref = Box::new(STExpr::new(ty.deref_or_pass(), zero_zero_loc.clone(), ExprKind::LocalVariable(
                        LocalVariable(input_name.clone())
                    )));
                    code_block.common_statements.push(
                        STStatement::If(zero_zero_loc.clone(), 
                            Box::new(STExpr::new(SLPType::bool(), zero_zero_loc.clone(), ExprKind::IsNull(
                                Box::new(STExpr::new(ty.clone(), zero_zero_loc.clone(), ExprKind::Deref(
                                    local_variable_ref.clone()
                                )))
                            ))), Box::new(STStatement::Empty()), 
                            Some(
                                Box::new(STStatement::CodeBlock(zero_zero_loc.clone(), 
                                    CodeBlock { common_statements: vec![
                                        STStatement::If(zero_zero_loc.clone(), Box::new(STExpr::new(SLPType::bool(), zero_zero_loc.clone(), 
                                        ExprKind::RefCountDecrease(local_variable_ref.clone()))),
                                        Box::new(STStatement::CodeBlock(zero_zero_loc.clone(), todo!())),
                                        Some(Box::new(STStatement::CodeBlock(zero_zero_loc.clone(), todo!())))
                                    )
                                    ], defer_statements: vec![] }))
                            )));
                } else {
                    todo!()
                }
                let func = Function { function_name: Id(format!("drop@{}", &tyname.0)), function_args: vec![( Id(input_name.clone()), ty.deref_or_pass())], return_arg: SLPType::PrimitiveType(SLPPrimitiveType::Void), body: code_block, temporary_variables: Default::default(), loc:  zero_zero_loc.clone()};

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