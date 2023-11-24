use std::{iter, fmt::format};

use text_trees::StringTreeNode;

use crate::ast::{ProgramFile, Declaration, Type, ArgDecl, Statement, Expr, Constant, VarDecl};

pub fn get_program_tree(pf: &ProgramFile ) -> StringTreeNode {
    StringTreeNode::with_child_nodes("FileRoot".to_string(), pf.declarations.iter().map(declaration))
}
pub fn declaration(decl: &Declaration)-> StringTreeNode {
    match decl {
        Declaration::Function(f) => 
        {
            let ret = &f.return_arg.ty;
            StringTreeNode::with_child_nodes(format!("fn {}" ,f.function_name), 
                f.function_args.iter().map(arg_decl).chain(vec![types(ret)]).chain(f.body.iter().map(statements))
            )
        },
        Declaration::ExternFunction(_) => todo!(),
        Declaration::TypeDeclSection(_) => todo!(),
    }   
}
pub fn arg_decl(a: &ArgDecl) -> StringTreeNode {
    StringTreeNode::with_child_nodes(format!("({})", a.names.join(", ")), vec![types(&a.ty.ty)].into_iter())
}
pub fn types(ty: &Type) -> StringTreeNode {
    match ty {
        Type::Primitive(x) => StringTreeNode::new(x.clone()),
        Type::Pointer(x) => StringTreeNode::with_child_nodes("Pointer of".to_string(), vec![types(&x)].into_iter()),
        Type::DynArray(x) => StringTreeNode::with_child_nodes("DynArray of".to_string(), vec![types(&x)].into_iter()),
        Type::FixedArray(b, e, x) => StringTreeNode::with_child_nodes(format!("Array[{b}..{e}] of"), vec![types(&x)].into_iter()),
    }
}
pub fn statements(st: &Statement) -> StringTreeNode {
    match st {
        Statement::CodeBlock(_, x) => StringTreeNode::with_child_nodes("Codeblock".to_string(), x.iter().map(statements)),
        Statement::Print(_, x) => StringTreeNode::with_child_nodes("Print".to_string(), vec![expressions(x)].into_iter()),
        Statement::Assignment(_, x, y) => todo!(),
        Statement::If(_, x, y, z) => todo!(),
        Statement::While(_, x, y) => todo!(),
        Statement::RepeatUntil(_, x, y) => todo!(),
        Statement::VarDecl(_, x) => vardecl(x),
        Statement::Empty() => todo!(),
        Statement::FunctionCall(_, x) => StringTreeNode::with_child_nodes("FunctionCall".to_string(), iter::once(expressions(&x.func)).chain(x.args.iter().map(expressions))),
    }
}
pub fn vardecl(vd: &VarDecl) -> StringTreeNode {
    match vd {
        VarDecl::Multiple(x, y) => StringTreeNode::with_child_nodes(format!("var ({})", x.join(", ")), vec![types(&y.ty)].into_iter()),
        VarDecl::ExplicitType(x, y, z) => StringTreeNode::with_child_nodes(format!("var ({})", x), vec![types(&y.ty), expressions(&z)].into_iter()),
        VarDecl::ImplicitType(x, y) => StringTreeNode::with_child_nodes(format!("var ({})", x), vec![expressions(&y)].into_iter()),
    }
}
pub fn constant(c: &Constant) -> StringTreeNode {
    StringTreeNode::new(format!("Constant {}",
    match c {
        Constant::String(x) => format!("String {x}"),
        Constant::Int64(x) => format!("Int64 {x}"),
        Constant::Float64(x) => format!("Float64 {x}"),
        Constant::Bool(x) => format!("Bool {x}"),
    }))
}
pub fn expressions(ex: &Expr) -> StringTreeNode {
    match ex {
        Expr::Constant(_, x) => constant(x),
        Expr::Ident(_, x) => StringTreeNode::new(format!("Id:{}", x)),
        Expr::OpBinPlus(_, x, y) => StringTreeNode::with_child_nodes("BinPlus".to_string(), vec![expressions(x), expressions(y)].into_iter()),
        Expr::OpBinMinus(_, _, _) => todo!(),
        Expr::OpBinAsterisk(_, x, y) => StringTreeNode::with_child_nodes("BinAsterisk".to_string(), vec![expressions(x), expressions(y)].into_iter()),
        Expr::OpBinSlash(_, _, _) => todo!(),
        Expr::OpBinDiv(_, _, _) => todo!(),
        Expr::OpBinMod(_, _, _) => todo!(),
        Expr::OpUnPlus(_, _) => todo!(),
        Expr::OpUnMinus(_, _) => todo!(),
        Expr::OpBinAnd(_, _, _) => todo!(),
        Expr::OpBinOr(_, _, _) => todo!(),
        Expr::OpBinXor(_, _, _) => todo!(),
        Expr::OpUnNot(_, _) => todo!(),
        Expr::OpBinShl(_, _, _) => todo!(),
        Expr::OpBinShr(_, _, _) => todo!(),
        Expr::OpBinLesser(_, _, _) => todo!(),
        Expr::OpBinGreater(_, _, _) => todo!(),
        Expr::OpBinLesserEq(_, _, _) => todo!(),
        Expr::OpBinGreaterEq(_, _, _) => todo!(),
        Expr::OpBinEq(_, _, _) => todo!(),
        Expr::OpBinNotEq(_, _, _) => todo!(),
        Expr::OpUnDeref(_, _) => todo!(),
        Expr::OpUnGetRef(_, _) => todo!(),
        Expr::OpFunctionCall(_, _) => todo!(),
        Expr::OpUnAs(_, _, _) => todo!(),
        Expr::OpDot(_, x, y) => StringTreeNode::with_child_nodes("BinDot".to_string(), vec![expressions(x), StringTreeNode::new(format!("{}", y))].into_iter()),
        Expr::OpNew(_, _, _) => todo!(),
    }
}