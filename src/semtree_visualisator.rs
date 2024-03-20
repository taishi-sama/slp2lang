use std::iter;

use text_trees::StringTreeNode;

use crate::semtree::{ExprKind, ExternFunction, Function, NumberLiteral, ProgramRoot, STExpr, STStatement, VarDecl};

pub fn get_program_root(root: &ProgramRoot) -> StringTreeNode {
    StringTreeNode::with_child_nodes(
        "root".into(),
        root.funcs
            .iter()
            .map(function)
            .chain(root.extern_funcs.iter().map(extern_function)),
    )
}
pub fn function(func: &Function) -> StringTreeNode {
    StringTreeNode::with_child_nodes(
        "function ".to_owned()
            + &func.function_name.0
            + " ("
            + &func
                .function_args
                .iter()
                .map(|x| format!("{:?} {}", x.1, x.0 .0))
                .reduce(|i1, i2| i1 + ", " + &i2)
                .unwrap_or_default()
            + "): "
            + &format!("{:?}", func.return_arg),
        func.body.iter().map(statement),
    )
}
pub fn extern_function(func: &ExternFunction) -> StringTreeNode {
    StringTreeNode::new(
        "function ".to_owned()
            + &func.function_name.0
            + " ("
            + &func
                .function_args
                .iter()
                .map(|x| format!("{:?} {}", x.1, x.0 .0))
                .reduce(|i1, i2| i1 + ", " + &i2)
                .unwrap_or_default()
            + "): "
            + &format!("{:?}", func.return_arg),
    )
}
pub fn statement_block(block: &[STStatement]) -> StringTreeNode {
    todo!()
}
pub fn statement(stmt: &STStatement) -> StringTreeNode {
    match stmt {
        STStatement::CodeBlock(_, _) => todo!(),
        STStatement::Print(_, e) => {
            StringTreeNode::with_child_nodes("print".to_owned(), iter::once(expr(&e)))
        }
        STStatement::FunctionCall(l, fc) => 
            StringTreeNode::with_child_nodes(fc.func.0.clone() + " -> " + &format!("{:?}", fc.ret_type), fc.args.iter().map(expr)),
        STStatement::Assignment(_, x, y) => StringTreeNode::with_child_nodes("Assign".to_string(), vec![expr(x), expr(y)].into_iter()),
        STStatement::If(_, _, _, _) => todo!(),
        STStatement::While(_, _, _) => todo!(),
        STStatement::RepeatUntil(_, _, _) => todo!(),
        STStatement::VarDecl(_, l) => vardecl(l),
        STStatement::Empty() => todo!(),
    }
}
fn vardecl(vd: &VarDecl) -> StringTreeNode {
    StringTreeNode::with_child_nodes(format!("var {}: {:?}", vd.id.0, vd.ty), vd.init_expr.as_ref().map(expr).into_iter())
}
pub fn expr(expression: &STExpr) -> StringTreeNode {
    match &expression.kind {
        ExprKind::LocalVariable(v) => StringTreeNode::new("Variable: ".to_string() + &v.0),
        ExprKind::TypeCast(_) => todo!(),
        ExprKind::NumberLiteral(l) => StringTreeNode::new(
            "NumberLiteral = ".to_string()
                + &match &l {
                    NumberLiteral::U32(_) => todo!(),
                    NumberLiteral::I32(i) => i.to_string(),
                    NumberLiteral::U64(_) => todo!(),
                    NumberLiteral::I64(i) => i.to_string(),
                    NumberLiteral::U16(_) => todo!(),
                    NumberLiteral::I16(_) => todo!(),
                    NumberLiteral::U8(i) => i.to_string(),
                    NumberLiteral::I8(i) => i.to_string(),
                },
        ),
        ExprKind::FunctionCall(fc) => 
        StringTreeNode::with_child_nodes(fc.func.0.clone() + " -> " + &format!("{:?}", fc.ret_type), fc.args.iter().map(expr)),
        ExprKind::BoolLiteral(b) =>  StringTreeNode::new(
            "BoolLiteral = ".to_string() + &b.to_string() ),

        ExprKind::PrimitiveIntComparation(_, _, _) => todo!(),
        ExprKind::PrimitiveIntBinOp(_, _, _) => todo!(),
        ExprKind::PrimitiveIntUnaryOp(_,  _) => todo!(),
        ExprKind::BoolBinOp(_, _, _) => todo!(),
        ExprKind::BoolUnaryOp(_, _) => todo!(),
        
    }
}
