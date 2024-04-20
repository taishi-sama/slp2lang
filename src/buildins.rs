use std::{collections::HashMap, sync::Arc};

use crate::{ast::Loc, semtree::{BuildInCall, CodeBlock, ExprKind, Function, LocalVariable, RhsExpr, STExpr, STStatement}, symbols::{GlobalSymbolResolver, Id}, types::{SLPPrimitiveType, SLPType}};

#[derive(Debug, Clone)]
pub struct BuildInModule {
    pub types_resolver: Arc<GlobalSymbolResolver>,
    pub drops: HashMap<SLPType, Id>,
    pub clones: HashMap<SLPType, Id>,
    pub buildins: HashMap<Id, Function>
}
impl BuildInModule {
    pub fn new(tyr: Arc<GlobalSymbolResolver>) -> Self {
        Self { types_resolver: tyr, drops: Default::default(), clones: Default::default(), buildins: Default::default() }
    }
    pub fn canonical_functions(id: &Id) -> Id {
        Id(format!("_slp2_buildins${}", id.0))
    }
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
                    let local_variable_ref = Box::new(STExpr::new(ty.wrap_autoderef_or_pass(), zero_zero_loc.clone(), ExprKind::LocalVariable(
                        LocalVariable(input_name.clone())
                    )));

                    let internal_drop = self.register_or_get_drop(&rc);
                    let mut internal_drop_codeblock = CodeBlock::new();
                    if let Some(id) = internal_drop {
                        internal_drop_codeblock.common_statements.push(
                            STStatement::BuildInCall(zero_zero_loc.clone(), BuildInCall{ func: id, args: vec![STExpr::new(rc.wrap_autoderef_or_pass(), zero_zero_loc, ExprKind::GetElementBehindReffedReferenceCounter(local_variable_ref.clone()))], ret_type: SLPType::void() }))
                    }
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
                                        Box::new(STStatement::CodeBlock(zero_zero_loc.clone(), internal_drop_codeblock)),
                                        None
                                    )
                                    ], defer_statements: vec![] }))
                            )));
                } else {
                    todo!()
                }
                let func_id = Id(format!("drop@{}", &tyname.0));
                let func = Function { function_name: func_id.clone(), function_args: vec![( Id(input_name.clone()), ty.wrap_autoderef_or_pass())], return_arg: SLPType::PrimitiveType(SLPPrimitiveType::Void), body: code_block, temporary_variables: Default::default(), loc:  zero_zero_loc.clone()};
                self.buildins.insert(func_id.clone(), func);
                self.drops.insert(ty.clone(), func_id.clone());
                Some(func_id)
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