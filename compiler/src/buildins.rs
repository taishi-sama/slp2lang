use std::{collections::HashMap, sync::Arc};

use crate::{
    ast::Loc,
    compiler::FileId,
    errors::CompilerErrors,
    semtree::{
        BuildInCall, CodeBlock, ExprKind, Function, LocalVariable, RhsExpr, STExpr, STStatement,
        SemanticTree, VarDecl,
    },
    symbols::{GlobalSymbolResolver, Id},
    types::{SLPPrimitiveType, SLPType},
};

#[derive(Debug, Clone)]
pub struct BuildInModule {
    pub types_resolver: Arc<GlobalSymbolResolver>,
    pub drops: HashMap<SLPType, Id>,
    pub clones: HashMap<SLPType, Id>,
    pub dyn_array_empty_constuctors: HashMap<SLPType, Id>,

    pub buildins: HashMap<Id, Function>,
}
impl BuildInModule {
    pub fn new(tyr: Arc<GlobalSymbolResolver>) -> Self {
        Self {
            types_resolver: tyr,
            drops: Default::default(),
            clones: Default::default(),
            buildins: Default::default(),
            dyn_array_empty_constuctors: Default::default(),
        }
    }
    pub fn canonical_functions(id: &Id) -> Id {
        Id(format!("_slp2_buildins${}", id.0))
    }
    pub fn register_or_get_drop(&mut self, ty: &SLPType) -> Result<Option<Id>, CompilerErrors> {
        if let Some(x) = self.drops.get(ty) {
            Ok(Some(x.clone()))
        } else {
            if ty.is_trivially_copiable() {
                Ok(None)
            } else {
                let zero_zero_loc = Loc::new(0, 0, FileId(0));
                assert!(ty.get_underlying_autoderef_type().is_none());
                let tyname = ty.normalized_name();
                let mut code_block = CodeBlock::new();
                let input_name = "input".to_string();
                code_block.common_statements.push(STStatement::VarDecl(
                    zero_zero_loc,
                    VarDecl {
                        id: LocalVariable(input_name.clone()),
                        ty: ty.wrap_autoderef_or_pass(),
                        init_expr: STExpr::new(
                            ty.wrap_autoderef_or_pass(),
                            zero_zero_loc,
                            ExprKind::FunctionArg(0),
                        ),
                    },
                ));

                let local_variable_ref = Box::new(STExpr::new(
                    ty.wrap_autoderef_or_pass(),
                    zero_zero_loc.clone(),
                    ExprKind::LocalVariable(LocalVariable(input_name.clone())),
                ));
                if let SLPType::RefCounter(rc) = ty {
                    let internal_drop = self.register_or_get_drop(&rc);
                    let mut internal_drop_codeblock = CodeBlock::new();
                    if let Some(id) = internal_drop? {
                        internal_drop_codeblock
                            .common_statements
                            .push(STStatement::BuildInCall(
                                zero_zero_loc.clone(),
                                BuildInCall {
                                    func: id,
                                    args: vec![STExpr::new(
                                        rc.wrap_autoderef_or_pass(),
                                        zero_zero_loc,
                                        ExprKind::GetElementBehindReffedReferenceCounter(
                                            local_variable_ref.clone(),
                                        ),
                                    )],
                                    ret_type: SLPType::void(),
                                },
                            ))
                    }
                    internal_drop_codeblock
                        .common_statements
                        .push(STStatement::MemoryFree(
                            zero_zero_loc.clone(),
                            Box::new(STExpr::new(
                                ty.clone(),
                                zero_zero_loc.clone(),
                                ExprKind::Deref(local_variable_ref.clone()),
                            )),
                        ));
                    code_block.common_statements.push(STStatement::If(
                        zero_zero_loc.clone(),
                        Box::new(STExpr::new(
                            SLPType::bool(),
                            zero_zero_loc.clone(),
                            ExprKind::IsNull(Box::new(STExpr::new(
                                ty.clone(),
                                zero_zero_loc.clone(),
                                ExprKind::Deref(local_variable_ref.clone()),
                            ))),
                        )),
                        Box::new(STStatement::Empty()),
                        Some(Box::new(STStatement::CodeBlock(
                            zero_zero_loc.clone(),
                            CodeBlock {
                                common_statements: vec![STStatement::If(
                                    zero_zero_loc.clone(),
                                    Box::new(STExpr::new(
                                        SLPType::bool(),
                                        zero_zero_loc.clone(),
                                        ExprKind::RefCountDecrease(local_variable_ref.clone()),
                                    )),
                                    Box::new(STStatement::CodeBlock(
                                        zero_zero_loc.clone(),
                                        internal_drop_codeblock,
                                    )),
                                    None,
                                )],
                                defer_statements: vec![],
                            },
                        ))),
                    ));
                } else if let SLPType::Struct(filename, id, _is_trivially) = ty {
                    let tyr = self.types_resolver.clone();
                    let str = tyr.get_struct(filename, id)?.unwrap();
                    for (i, (_field, int_ty)) in str.fields.iter().enumerate() {
                        let internal_drop = self.register_or_get_drop(&int_ty)?;
                        if let Some(dropper) = internal_drop {
                            let field_ref_expr = STExpr::new(
                                int_ty.wrap_autoderef_or_pass(),
                                zero_zero_loc.clone(),
                                ExprKind::GetElementRefInReffedRecord(
                                    local_variable_ref.clone(),
                                    i.try_into().unwrap(),
                                ),
                            );
                            code_block.common_statements.push(STStatement::BuildInCall(
                                zero_zero_loc.clone(),
                                BuildInCall {
                                    func: dropper,
                                    args: vec![field_ref_expr],
                                    ret_type: SLPType::void(),
                                },
                            ))
                        }
                    }
                } else if let SLPType::FixedArray {
                    size,
                    index_offset: _,
                    ty: int_ty,
                } = ty
                {
                    let internal_drop = self.register_or_get_drop(&int_ty)?;

                    if let Some(dropper) = internal_drop {
                        let counter = "counter".to_string();
                        let counter_decl = STStatement::VarDecl(
                            zero_zero_loc.clone(),
                            VarDecl {
                                id: LocalVariable(counter.clone()),
                                ty: SLPType::isize(),
                                init_expr: STExpr::new(
                                    SLPType::isize(),
                                    zero_zero_loc,
                                    ExprKind::NumberLiteral(crate::semtree::NumberLiteral::ISize(
                                        0,
                                    )),
                                ),
                            },
                        );
                        let size = SemanticTree::build_int_constant(
                            *size,
                            SLPPrimitiveType::ISize,
                            zero_zero_loc.clone(),
                        )
                        .unwrap();
                        let const_1 = SemanticTree::build_int_constant(
                            1,
                            SLPPrimitiveType::ISize,
                            zero_zero_loc.clone(),
                        )
                        .unwrap();

                        code_block.common_statements.push(counter_decl);
                        let counter_var = STExpr::new(
                            SLPType::isize(),
                            zero_zero_loc.clone(),
                            ExprKind::LocalVariable(LocalVariable(counter.clone())),
                        );
                        let counter_var_ref = STExpr::new(
                            SLPType::isize().wrap_autoderef_or_pass(),
                            zero_zero_loc.clone(),
                            ExprKind::GetLocalVariableRef(LocalVariable(counter.clone())),
                        );

                        let cond = STExpr::new(
                            SLPType::bool(),
                            zero_zero_loc.clone(),
                            ExprKind::PrimitiveIntComparation(
                                Box::new(counter_var.clone()),
                                Box::new(size),
                                crate::semtree::ComparationKind::LesserThan,
                            ),
                        );
                        let mut while_body = CodeBlock::new();
                        let element = STExpr::new(
                            int_ty.wrap_autoderef_or_pass(),
                            zero_zero_loc.clone(),
                            ExprKind::GetElementRefInReffedArray(
                                local_variable_ref,
                                Box::new(counter_var.clone()),
                            ),
                        );
                        let drop_call = STStatement::BuildInCall(
                            zero_zero_loc.clone(),
                            BuildInCall {
                                func: dropper,
                                args: vec![element],
                                ret_type: SLPType::void(),
                            },
                        );

                        let counter_increment = STStatement::Assignment(
                            zero_zero_loc.clone(),
                            Box::new(RhsExpr {
                                required_type: SLPType::isize(),
                                loc: zero_zero_loc.clone(),
                                kind: crate::semtree::RhsKind::Deref(counter_var_ref),
                            }),
                            None,
                            Box::new(STExpr::new(
                                SLPType::isize(),
                                zero_zero_loc.clone(),
                                ExprKind::PrimitiveIntBinOp(
                                    Box::new(counter_var.clone()),
                                    Box::new(const_1),
                                    crate::semtree::IntBinOp::Add,
                                ),
                            )),
                        );
                        while_body.common_statements.push(drop_call);
                        while_body.common_statements.push(counter_increment);
                        code_block.common_statements.push(STStatement::While(
                            zero_zero_loc.clone(),
                            Box::new(cond),
                            Box::new(STStatement::CodeBlock(zero_zero_loc.clone(), while_body)),
                        ));
                    }
                } else if let SLPType::DynArray(int_ty) = ty {
                    let internal_drop = self.register_or_get_drop(&int_ty)?;
                    if let Some(dropper) = internal_drop {
                        let counter = "counter".to_string();
                        let counter_decl = STStatement::VarDecl(
                            zero_zero_loc.clone(),
                            VarDecl {
                                id: LocalVariable(counter.clone()),
                                ty: SLPType::usize(),
                                init_expr: SemanticTree::build_int_constant(
                                    0,
                                    SLPPrimitiveType::USize,
                                    zero_zero_loc.clone(),
                                )?,
                            },
                        );
                        let size = STExpr::new(
                            SLPType::usize(),
                            zero_zero_loc.clone(),
                            ExprKind::DynArrayLongLen(Box::new(STExpr::new(
                                ty.clone(),
                                zero_zero_loc.clone(),
                                ExprKind::Deref(local_variable_ref.clone()),
                            ))),
                        );
                        let const_1 = SemanticTree::build_int_constant(
                            1,
                            SLPPrimitiveType::ISize,
                            zero_zero_loc.clone(),
                        )
                        .unwrap();

                        code_block.common_statements.push(counter_decl);
                        let counter_var = STExpr::new(
                            SLPType::usize(),
                            zero_zero_loc.clone(),
                            ExprKind::LocalVariable(LocalVariable(counter.clone())),
                        );
                        let counter_var_ref = STExpr::new(
                            SLPType::usize().wrap_autoderef_or_pass(),
                            zero_zero_loc.clone(),
                            ExprKind::GetLocalVariableRef(LocalVariable(counter.clone())),
                        );

                        let cond = STExpr::new(
                            SLPType::bool(),
                            zero_zero_loc.clone(),
                            ExprKind::PrimitiveIntComparation(
                                Box::new(counter_var.clone()),
                                Box::new(size),
                                crate::semtree::ComparationKind::LesserThan,
                            ),
                        );
                        let mut while_body = CodeBlock::new();
                        let element = STExpr::new(
                            int_ty.wrap_autoderef_or_pass(),
                            zero_zero_loc.clone(),
                            ExprKind::GetElementRefInReffedArray(
                                local_variable_ref.clone(),
                                Box::new(counter_var.clone()),
                            ),
                        );
                        let drop_call = STStatement::BuildInCall(
                            zero_zero_loc.clone(),
                            BuildInCall {
                                func: dropper,
                                args: vec![element],
                                ret_type: SLPType::void(),
                            },
                        );

                        let counter_increment = STStatement::Assignment(
                            zero_zero_loc.clone(),
                            Box::new(RhsExpr {
                                required_type: SLPType::usize(),
                                loc: zero_zero_loc.clone(),
                                kind: crate::semtree::RhsKind::Deref(counter_var_ref),
                            }),
                            None,
                            Box::new(STExpr::new(
                                SLPType::usize(),
                                zero_zero_loc.clone(),
                                ExprKind::PrimitiveIntBinOp(
                                    Box::new(counter_var.clone()),
                                    Box::new(const_1),
                                    crate::semtree::IntBinOp::Add,
                                ),
                            )),
                        );
                        while_body.common_statements.push(drop_call);
                        while_body.common_statements.push(counter_increment);
                        code_block.common_statements.push(STStatement::While(
                            zero_zero_loc.clone(),
                            Box::new(cond),
                            Box::new(STStatement::CodeBlock(zero_zero_loc.clone(), while_body)),
                        ));
                    }
                    code_block.common_statements.push(STStatement::MemoryFree(
                        zero_zero_loc.clone(),
                        Box::new(STExpr::new(
                            ty.clone(),
                            zero_zero_loc.clone(),
                            ExprKind::Deref(local_variable_ref.clone()),
                        )),
                    ));
                } else {
                    unreachable!()
                }
                let func_id = Id(format!("drop@{}", &tyname.0));
                let func = Function {
                    function_name: func_id.clone(),
                    function_args: vec![(Id(input_name.clone()), ty.wrap_autoderef_or_pass())],
                    return_arg: SLPType::PrimitiveType(SLPPrimitiveType::Void),
                    body: code_block,
                    temporary_variables: Default::default(),
                    loc: zero_zero_loc.clone(),
                };
                self.buildins.insert(func_id.clone(), func);
                self.drops.insert(ty.clone(), func_id.clone());
                Ok(Some(func_id))
            }
        }
    }
    pub fn register_or_get_clone(&mut self, ty: &SLPType) -> Result<Option<Id>, CompilerErrors> {
        if let Some(x) = self.clones.get(ty) {
            Ok(Some(x.clone()))
        } else {
            if ty.is_trivially_copiable() {
                Ok(None)
            } else {
                let zero_zero_loc = Loc::new(0, 0, FileId(0));
                assert!(ty.get_underlying_autoderef_type().is_none());
                let tyname = ty.normalized_name();
                let mut code_block = CodeBlock::new();
                let input_name = "input".to_string();
                let local_variable_ref = Box::new(STExpr::new(
                    ty.wrap_autoderef_or_pass(),
                    zero_zero_loc.clone(),
                    ExprKind::LocalVariable(LocalVariable(input_name.clone())),
                ));
                code_block.common_statements.push(STStatement::VarDecl(
                    zero_zero_loc,
                    VarDecl {
                        id: LocalVariable(input_name.clone()),
                        ty: ty.wrap_autoderef_or_pass(),
                        init_expr: STExpr::new(
                            ty.wrap_autoderef_or_pass(),
                            zero_zero_loc,
                            ExprKind::FunctionArg(0),
                        ),
                    },
                ));
                let mut expr: STExpr = STExpr::new(ty.clone(), zero_zero_loc, ExprKind::Default);
                if let SLPType::RefCounter(_rc) = ty {
                    expr = STExpr::new(
                        ty.clone(),
                        zero_zero_loc,
                        ExprKind::RefCountIncrease(local_variable_ref.clone()),
                    );
                }
                code_block.common_statements.push(STStatement::VarDecl(
                    zero_zero_loc,
                    VarDecl {
                        id: LocalVariable("Result".to_string()),
                        ty: ty.clone(),
                        init_expr: expr,
                    },
                ));

                let func_id = Id(format!("clone@{}", &tyname.0));
                let func = Function {
                    function_name: func_id.clone(),
                    function_args: vec![(Id(input_name.clone()), ty.wrap_autoderef_or_pass())],
                    return_arg: ty.clone(),
                    body: code_block,
                    temporary_variables: Default::default(),
                    loc: zero_zero_loc.clone(),
                };
                self.buildins.insert(func_id.clone(), func);
                self.clones.insert(ty.clone(), func_id.clone());
                Ok(Some(func_id))
            }
        }
    }
    pub fn register_or_get_dyn_array_empty_constuctors(
        &mut self,
        array_type: &SLPType,
    ) -> Result<Id, CompilerErrors> {
        let zero_zero_loc = Loc::new(0, 0, FileId(0));

        if let SLPType::DynArray(ty) = array_type {
            let mut code_block = CodeBlock::new();

            let size_var = "input".to_string();
            code_block.common_statements.push(STStatement::VarDecl(
                zero_zero_loc,
                VarDecl {
                    id: LocalVariable(size_var.clone()),
                    ty: SLPType::usize(),
                    init_expr: STExpr::new(
                        SLPType::usize(),
                        zero_zero_loc,
                        ExprKind::FunctionArg(0),
                    ),
                },
            ));
            let size_var_value = STExpr::new(
                SLPType::usize(),
                zero_zero_loc.clone(),
                ExprKind::LocalVariable(LocalVariable(size_var.clone())),
            );

            let arr_var = "Result".to_string();
            code_block.common_statements.push(STStatement::VarDecl(
                zero_zero_loc,
                VarDecl {
                    id: LocalVariable(arr_var.clone()),
                    ty: array_type.clone(),
                    init_expr: STExpr::new(
                        array_type.clone(),
                        zero_zero_loc.clone(),
                        ExprKind::ConstructUninitizedDynArray(Box::new(size_var_value.clone())),
                    ),
                },
            ));
            let arr_value_ref = STExpr::new(
                array_type.wrap_autoderef_or_pass(),
                zero_zero_loc.clone(),
                ExprKind::GetLocalVariableRef(LocalVariable(arr_var.clone())),
            );

            let tyname = array_type.normalized_name();
            let func_id = Id(format!("build_empty@{}", &tyname.0));

            let counter = "counter".to_string();
            let counter_decl = STStatement::VarDecl(
                zero_zero_loc.clone(),
                VarDecl {
                    id: LocalVariable(counter.clone()),
                    ty: SLPType::usize(),
                    init_expr: SemanticTree::build_int_constant(
                        0,
                        SLPPrimitiveType::USize,
                        zero_zero_loc.clone(),
                    )?,
                },
            );
            code_block.common_statements.push(counter_decl);
            let counter_var = STExpr::new(
                SLPType::usize(),
                zero_zero_loc.clone(),
                ExprKind::LocalVariable(LocalVariable(counter.clone())),
            );
            let counter_var_ref = STExpr::new(
                SLPType::usize().wrap_autoderef_or_pass(),
                zero_zero_loc.clone(),
                ExprKind::GetLocalVariableRef(LocalVariable(counter.clone())),
            );

            let const_1 =
                SemanticTree::build_int_constant(1, SLPPrimitiveType::USize, zero_zero_loc.clone())
                    .unwrap();
            let cond = STExpr::new(
                SLPType::bool(),
                zero_zero_loc.clone(),
                ExprKind::PrimitiveIntComparation(
                    Box::new(counter_var.clone()),
                    Box::new(size_var_value),
                    crate::semtree::ComparationKind::LesserThan,
                ),
            );
            let mut internal_body = CodeBlock::new();
            let elem_ref = STExpr::new(
                ty.wrap_autoderef_or_pass(),
                zero_zero_loc,
                ExprKind::GetElementRefInReffedArray(
                    Box::new(arr_value_ref),
                    Box::new(counter_var.clone()),
                ),
            );
            let assign = STStatement::Assignment(
                zero_zero_loc.clone(),
                Box::new(RhsExpr {
                    required_type: *ty.clone(),
                    loc: zero_zero_loc.clone(),
                    kind: crate::semtree::RhsKind::Deref(elem_ref),
                }),
                None,
                Box::new(STExpr::new(
                    *ty.clone(),
                    zero_zero_loc.clone(),
                    ExprKind::Default,
                )),
            );
            let counter_increment = STStatement::Assignment(
                zero_zero_loc.clone(),
                Box::new(RhsExpr {
                    required_type: SLPType::usize(),
                    loc: zero_zero_loc.clone(),
                    kind: crate::semtree::RhsKind::Deref(counter_var_ref),
                }),
                None,
                Box::new(STExpr::new(
                    SLPType::usize(),
                    zero_zero_loc.clone(),
                    ExprKind::PrimitiveIntBinOp(
                        Box::new(counter_var.clone()),
                        Box::new(const_1),
                        crate::semtree::IntBinOp::Add,
                    ),
                )),
            );
            internal_body.common_statements.push(assign);
            internal_body.common_statements.push(counter_increment);

            code_block.common_statements.push(STStatement::While(
                zero_zero_loc.clone(),
                Box::new(cond),
                Box::new(STStatement::CodeBlock(zero_zero_loc.clone(), internal_body)),
            ));

            let func = Function {
                function_name: func_id.clone(),
                function_args: vec![(
                    Id(size_var.clone()),
                    SLPType::PrimitiveType(SLPPrimitiveType::USize),
                )],
                return_arg: array_type.clone(),
                body: code_block,
                temporary_variables: Default::default(),
                loc: zero_zero_loc.clone(),
            };
            self.buildins.insert(func_id.clone(), func);
            Ok(func_id)
        } else {
            todo!("Internal compiler bug")
        }
    }
    pub fn build_or_get_stringbuilder(&mut self) -> Result<Id, CompilerErrors>  {
        let stringbuilder_id = Id("StringBuilder".to_owned());
        if self.buildins.contains_key(&stringbuilder_id) {
            return Ok(stringbuilder_id);
        } else {
            let zero_zero_loc = Loc::new(0, 0, FileId(0));
            let mut code_block = CodeBlock::new();

            let size_var = "input".to_string();
            code_block.common_statements.push(STStatement::VarDecl(
                zero_zero_loc,
                VarDecl {
                    id: LocalVariable(size_var.clone()),
                    ty: SLPType::usize(),
                    init_expr: STExpr::new(
                        SLPType::usize(),
                        zero_zero_loc,
                        ExprKind::FunctionArg(0),
                    ),
                },
            ));
            let source_var = "strlit".to_string();
            code_block.common_statements.push(STStatement::VarDecl(
                zero_zero_loc,
                VarDecl {
                    id: LocalVariable(source_var.clone()),
                    ty: SLPType::PrimitiveType(SLPPrimitiveType::StringLiteral(u32::MAX)).wrap_autoderef_or_pass(),
                    init_expr: STExpr::new(
                        SLPType::PrimitiveType(SLPPrimitiveType::StringLiteral(u32::MAX)).wrap_autoderef_or_pass(),
                        zero_zero_loc,
                        ExprKind::FunctionArg(1),
                    ),
                },
            ));
            let strlit_ref_value = STExpr::new(
                SLPType::PrimitiveType(SLPPrimitiveType::StringLiteral(u32::MAX)).wrap_autoderef_or_pass(),
                zero_zero_loc.clone(),
                ExprKind::LocalVariable(LocalVariable(source_var.clone())),
            );
            let size_var_value = STExpr::new(
                SLPType::usize(),
                zero_zero_loc.clone(),
                ExprKind::LocalVariable(LocalVariable(size_var.clone())),
            );
            let ty = Box::new(SLPType::PrimitiveType(SLPPrimitiveType::Char));
            let array_type = SLPType::DynArray(ty.clone());
            let arr_var = "Result".to_string();
            code_block.common_statements.push(STStatement::VarDecl(
                zero_zero_loc,
                VarDecl {
                    id: LocalVariable(arr_var.clone()),
                    ty: array_type.clone(),
                    init_expr: STExpr::new(
                        array_type.clone(),
                        zero_zero_loc.clone(),
                        ExprKind::ConstructUninitizedDynArray(Box::new(size_var_value.clone())),
                    ),
                },
            ));
            let arr_value_ref = STExpr::new(
                array_type.wrap_autoderef_or_pass(),
                zero_zero_loc.clone(),
                ExprKind::GetLocalVariableRef(LocalVariable(arr_var.clone())),
            );

            let tyname = array_type.normalized_name();
            let func_id = Id(format!("build_empty@{}", &tyname.0));

            let counter = "counter".to_string();
            let counter_decl = STStatement::VarDecl(
                zero_zero_loc.clone(),
                VarDecl {
                    id: LocalVariable(counter.clone()),
                    ty: SLPType::usize(),
                    init_expr: SemanticTree::build_int_constant(
                        0,
                        SLPPrimitiveType::USize,
                        zero_zero_loc.clone(),
                    )?,
                },
            );
            code_block.common_statements.push(counter_decl);
            let counter_var = STExpr::new(
                SLPType::usize(),
                zero_zero_loc.clone(),
                ExprKind::LocalVariable(LocalVariable(counter.clone())),
            );
            let counter_var_ref = STExpr::new(
                SLPType::usize().wrap_autoderef_or_pass(),
                zero_zero_loc.clone(),
                ExprKind::GetLocalVariableRef(LocalVariable(counter.clone())),
            );

            let const_1 =
                SemanticTree::build_int_constant(1, SLPPrimitiveType::USize, zero_zero_loc.clone())
                    .unwrap();
            let cond = STExpr::new(
                SLPType::bool(),
                zero_zero_loc.clone(),
                ExprKind::PrimitiveIntComparation(
                    Box::new(counter_var.clone()),
                    Box::new(size_var_value),
                    crate::semtree::ComparationKind::LesserThan,
                ),
            );
            let mut internal_body = CodeBlock::new();
            let elem_ref = STExpr::new(
                ty.wrap_autoderef_or_pass(),
                zero_zero_loc,
                ExprKind::GetElementRefInReffedArray(
                    Box::new(arr_value_ref),
                    Box::new(counter_var.clone()),
                ),
            );
            let char_value =STExpr::new(*ty.clone(), zero_zero_loc, ExprKind::Deref( Box::new(STExpr::new(
                ty.wrap_autoderef_or_pass(),
                zero_zero_loc,
                ExprKind::GetElementRefInReffedArray(
                    Box::new(strlit_ref_value),
                    Box::new(counter_var.clone()),
                ),
            ))));
            let assign = STStatement::Assignment(
                zero_zero_loc.clone(),
                Box::new(RhsExpr {
                    required_type: *ty.clone(),
                    loc: zero_zero_loc.clone(),
                    kind: crate::semtree::RhsKind::Deref(elem_ref),
                }),
                None,
                Box::new(char_value),
            );
            let counter_increment = STStatement::Assignment(
                zero_zero_loc.clone(),
                Box::new(RhsExpr {
                    required_type: SLPType::usize(),
                    loc: zero_zero_loc.clone(),
                    kind: crate::semtree::RhsKind::Deref(counter_var_ref),
                }),
                None,
                Box::new(STExpr::new(
                    SLPType::usize(),
                    zero_zero_loc.clone(),
                    ExprKind::PrimitiveIntBinOp(
                        Box::new(counter_var.clone()),
                        Box::new(const_1),
                        crate::semtree::IntBinOp::Add,
                    ),
                )),
            );
            internal_body.common_statements.push(assign);
            internal_body.common_statements.push(counter_increment);

            code_block.common_statements.push(STStatement::While(
                zero_zero_loc.clone(),
                Box::new(cond),
                Box::new(STStatement::CodeBlock(zero_zero_loc.clone(), internal_body)),
            ));

            let func = Function {
                function_name: func_id.clone(),
                function_args: vec![(
                    Id(size_var.clone()),
                    SLPType::PrimitiveType(SLPPrimitiveType::USize),
                ),
                (
                    Id(source_var.clone()),
                    SLPType::PrimitiveType(SLPPrimitiveType::StringLiteral(u32::MAX)).wrap_autoderef_or_pass(),
                )
                ],
                return_arg: array_type.clone(),
                body: code_block,
                temporary_variables: Default::default(),
                loc: zero_zero_loc.clone(),
            };
            self.buildins.insert(stringbuilder_id.clone(), func);
            Ok(stringbuilder_id)
        }
    }
}
