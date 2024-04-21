use std::{collections::HashMap, sync::Arc};

use crate::{ast::Loc, errors::SemTreeBuildErrors, semtree::{BuildInCall, CodeBlock, ExprKind, Function, LocalVariable, RhsExpr, STExpr, STStatement, VarDecl}, symbols::{GlobalSymbolResolver, Id}, types::{SLPPrimitiveType, SLPType}};

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
    pub fn register_or_get_drop(&mut self, ty: &SLPType) -> Result<Option<Id>, SemTreeBuildErrors> {
        if let Some(x) = self.drops.get(ty) {
            Ok(Some(x.clone()))
        } else {
            if ty.is_trivially_copiable() { 
                Ok(None)
            } 
            else {
                let zero_zero_loc = Loc::new(0, 0);
                assert!(ty.get_underlying_autoderef_type().is_none());
                let tyname = ty.normalized_name(); 
                let mut code_block = CodeBlock::new();
                let input_name = "input".to_string();
                code_block.common_statements.push(STStatement::VarDecl(zero_zero_loc, VarDecl{ id: LocalVariable(input_name.clone()), ty: ty.wrap_autoderef_or_pass(), init_expr: STExpr::new(ty.wrap_autoderef_or_pass(), zero_zero_loc, ExprKind::FunctionArg(0)) }));

                let local_variable_ref = Box::new(STExpr::new(ty.wrap_autoderef_or_pass(), zero_zero_loc.clone(), ExprKind::LocalVariable(
                    LocalVariable(input_name.clone())
                )));
                if let SLPType::RefCounter(rc) = ty {
                    

                    let internal_drop = self.register_or_get_drop(&rc);
                    let mut internal_drop_codeblock = CodeBlock::new();
                    if let Some(id) = internal_drop? {
                        internal_drop_codeblock.common_statements.push(
                            STStatement::BuildInCall(zero_zero_loc.clone(), BuildInCall{ func: id, args: vec![STExpr::new(rc.wrap_autoderef_or_pass(), zero_zero_loc, ExprKind::GetElementBehindReffedReferenceCounter(local_variable_ref.clone()))], ret_type: SLPType::void() }))
                    }
                    internal_drop_codeblock.common_statements.push(
                        STStatement::MemoryFree(zero_zero_loc.clone(), 
                            Box::new(STExpr::new(ty.clone(), zero_zero_loc.clone(), ExprKind::Deref(
                                local_variable_ref.clone()
                            )))
                        )
                    );
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
                } 
                else if let SLPType::Struct(filename, id, is_trivially) = ty {
                    let tyr = self.types_resolver.clone();
                    let str = tyr.get_struct(filename, id)?.unwrap();
                    for (i, (field, int_ty)) in str.fields.iter().enumerate() {
                        let internal_drop = self.register_or_get_drop(&int_ty)?;
                        if let Some(dropper) = internal_drop {
                            let field_ref_expr = STExpr::new(int_ty.wrap_autoderef_or_pass(), zero_zero_loc.clone(), ExprKind::GetElementRefInReffedRecord(local_variable_ref.clone(), i.try_into().unwrap()));
                            code_block.common_statements.push(
                            STStatement::BuildInCall(zero_zero_loc.clone(), BuildInCall{ func: dropper, args: vec![field_ref_expr], ret_type: SLPType::void() }))

                        }

                    }
                } else if let SLPType::FixedArray{ size, index_offset, ty: int_ty } = ty{
                    let internal_drop = self.register_or_get_drop(&int_ty)?;
                    
                    if let Some(dropper) = internal_drop {
                        let counter = "counter".to_string();
                        let id = STStatement::VarDecl(zero_zero_loc.clone(), VarDecl { id: LocalVariable(counter.clone()), ty: SLPType::isize(), init_expr: STExpr::new(SLPType::isize(), zero_zero_loc, ExprKind::NumberLiteral(crate::semtree::NumberLiteral::ISize(0)))});
                        let size = STExpr::new(SLPType::isize(), zero_zero_loc, ExprKind::NumberLiteral(crate::semtree::NumberLiteral::ISize((*size).try_into().unwrap())));
                        let const_1 = STExpr::new(SLPType::isize(), zero_zero_loc, ExprKind::NumberLiteral(crate::semtree::NumberLiteral::ISize(1)));
                        
                        code_block.common_statements.push(id);
                        let counter_var = STExpr::new(SLPType::isize(), zero_zero_loc.clone(), ExprKind::LocalVariable(LocalVariable(counter.clone())));
                        let counter_var_ref = STExpr::new(SLPType::isize().wrap_autoderef_or_pass(), zero_zero_loc.clone(), ExprKind::GetLocalVariableRef(LocalVariable(counter.clone())));
                        
                        let cond = STExpr::new(SLPType::bool(), zero_zero_loc.clone(), ExprKind::PrimitiveIntComparation(Box::new(counter_var.clone()), Box::new(size), crate::semtree::ComparationKind::LesserThan));
                        let mut while_body = CodeBlock::new();
                        let element = STExpr::new(int_ty.wrap_autoderef_or_pass(), zero_zero_loc.clone(), ExprKind::GetElementRefInReffedArray(local_variable_ref, Box::new(counter_var.clone())));
                        let drop_call = STStatement::BuildInCall(zero_zero_loc.clone(), BuildInCall{ func: dropper, args: vec![element], ret_type: SLPType::void() });

                        let counter_increment = STStatement::Assignment(zero_zero_loc.clone(),
                            Box::new(RhsExpr { required_type: SLPType::isize(), loc: zero_zero_loc.clone(), kind: crate::semtree::RhsKind::Deref(counter_var_ref)}), None, 
                            Box::new(STExpr::new(SLPType::isize(), zero_zero_loc.clone(), ExprKind::PrimitiveIntBinOp(Box::new(counter_var.clone()), Box::new(const_1), crate::semtree::IntBinOp::Add)))
                        );
                        while_body.common_statements.push(drop_call);
                        while_body.common_statements.push(counter_increment);
                        code_block.common_statements.push(STStatement::While(zero_zero_loc.clone(), Box::new(cond), Box::new(STStatement::CodeBlock(zero_zero_loc.clone(), while_body))));
                    }
                }
                let func_id = Id(format!("drop@{}", &tyname.0));
                let func = Function { function_name: func_id.clone(), function_args: vec![( Id(input_name.clone()), ty.wrap_autoderef_or_pass())], return_arg: SLPType::PrimitiveType(SLPPrimitiveType::Void), body: code_block, temporary_variables: Default::default(), loc:  zero_zero_loc.clone()};
                self.buildins.insert(func_id.clone(), func);
                self.drops.insert(ty.clone(), func_id.clone());
                Ok(Some(func_id))
            }
        }
    }
    pub fn register_or_get_clone(&mut self, ty: &SLPType) -> Result<Option<Id>, SemTreeBuildErrors> {
        if let Some(x) = self.clones.get(ty) {
            Ok(Some(x.clone()))
        } else {
            if ty.is_trivially_copiable() { 
                Ok(None)
            } 
            else {
                let zero_zero_loc = Loc::new(0, 0);
                assert!(ty.get_underlying_autoderef_type().is_none());
                let tyname = ty.normalized_name(); 
                let mut code_block = CodeBlock::new();
                let input_name = "input".to_string();
                let local_variable_ref = Box::new(STExpr::new(ty.wrap_autoderef_or_pass(), zero_zero_loc.clone(), ExprKind::LocalVariable(
                    LocalVariable(input_name.clone())
                )));
                code_block.common_statements.push(STStatement::VarDecl(zero_zero_loc, VarDecl{ id: LocalVariable(input_name.clone()), ty: ty.wrap_autoderef_or_pass(), init_expr: STExpr::new(ty.wrap_autoderef_or_pass(), zero_zero_loc, ExprKind::FunctionArg(0)) }));
                let mut expr : STExpr = STExpr::new(ty.clone(), zero_zero_loc, ExprKind::Default);
                if let SLPType::RefCounter(rc) = ty {
                    expr = STExpr::new(ty.clone(), zero_zero_loc, ExprKind::RefCountIncrease(local_variable_ref.clone()));
                }
                code_block.common_statements.push(STStatement::VarDecl(zero_zero_loc, VarDecl{ id: LocalVariable("Result".to_string()), ty: ty.clone(), init_expr: expr }));

                let func_id = Id(format!("clone@{}", &tyname.0));
                let func = Function { function_name: func_id.clone(), function_args: vec![( Id(input_name.clone()), ty.wrap_autoderef_or_pass())], return_arg: ty.clone(), body: code_block, temporary_variables: Default::default(), loc:  zero_zero_loc.clone()};
                self.buildins.insert(func_id.clone(), func);
                self.clones.insert(ty.clone(), func_id.clone());
                Ok(Some(func_id))
            }
        }
    }
    
}