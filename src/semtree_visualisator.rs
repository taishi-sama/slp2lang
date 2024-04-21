use std::iter;

use text_trees::StringTreeNode;

use crate::semtree::{
    CodeBlock, ExprKind, ExternFunction, Function, NumberLiteral, ProgramRoot, RhsExpr, STExpr, STStatement, VarDecl
};

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
        vec![statement_block(&func.body)].into_iter(),
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
pub fn statement_block(block: &CodeBlock) -> StringTreeNode {
    StringTreeNode::with_child_nodes(
        "Codeblock".to_string(),
        vec![StringTreeNode::with_child_nodes("Common statements: ".to_owned(), block.common_statements.iter().map(|x|statement(x))),
        StringTreeNode::with_child_nodes("Defer statements(in call order): ".to_owned(), block.defer_statements.iter().rev().map(|x|statement(x)))].into_iter()
    )
}
pub fn statement(stmt: &STStatement) -> StringTreeNode {
    match stmt {
        STStatement::CodeBlock(_, v) => statement_block(v),
        STStatement::Print(_, e) => {
            StringTreeNode::with_child_nodes("print".to_owned(), iter::once(expr(&e)))
        }
        STStatement::FunctionCall(_, fc) => StringTreeNode::with_child_nodes(
            fc.func.0.clone() + " -> " + &format!("{:?}", fc.ret_type),
            fc.args.iter().map(expr),
        ),
        STStatement::Assignment(_, x, z, y) => StringTreeNode::with_child_nodes(
            "Assign".to_string(),
            vec![rhs(x), expr(y)].into_iter(),
        ),
        STStatement::If(l, condition, mb, ab) => StringTreeNode::with_child_nodes(
            "If ".to_string(),
            vec![expr(condition), statement(mb)].into_iter().chain(ab.as_ref().map(|x|statement(&x))),
        ),
        STStatement::While(_, cond, block) => StringTreeNode::with_child_nodes(
            "While ".to_string(),
            vec![expr(cond), statement(block)].into_iter(),
        ),
        STStatement::RepeatUntil(_, _, _) => todo!(),
        STStatement::VarDecl(_, l) => vardecl(l),
        STStatement::Empty() => StringTreeNode::new("*Empty*".to_string()),
        STStatement::DeferHint(_, num) => StringTreeNode::new(format!("Location of {num}'th defer statement in this code block")),
        STStatement::BuildInCall(_, fc) => StringTreeNode::with_child_nodes(
            fc.func.0.clone() + " -> " + &format!("{:?}", fc.ret_type),
            fc.args.iter().map(expr),
        ),
        STStatement::MemoryFree(_, e) => StringTreeNode::with_child_nodes(
            "MemoryFree ".to_string(),
            vec![expr(e)].into_iter()),
    }
}
fn vardecl(vd: &VarDecl) -> StringTreeNode {
    StringTreeNode::with_child_nodes(
        format!("var {}: {:?}", vd.id.0, vd.ty),
        vec![expr(&vd.init_expr)].into_iter(),
    )
}
pub fn rhs(expression: &RhsExpr) -> StringTreeNode {
    match &expression.kind {

        crate::semtree::RhsKind::Deref(dt) => {
            StringTreeNode::with_child_nodes("Defer of ".to_owned(), vec![expr(dt)].into_iter())
        }
    }
}
pub fn expr(expression: &STExpr) -> StringTreeNode {
    match &expression.kind {
        ExprKind::LocalVariable(v) => StringTreeNode::new("Variable: ".to_string() + &v.0),
        ExprKind::TypeCast(ex, typecast) => StringTreeNode::with_child_nodes(
            format!("Typecast: {typecast:?}"),
            vec![expr(ex)].into_iter(),
        ),
        ExprKind::NumberLiteral(l) => StringTreeNode::new(
            "NumberLiteral = ".to_string()
                + &match &l {
                    NumberLiteral::U32(i) => i.to_string(),
                    NumberLiteral::I32(i) => i.to_string(),
                    NumberLiteral::U64(i) => i.to_string(),
                    NumberLiteral::I64(i) => i.to_string(),
                    NumberLiteral::U16(i) => i.to_string(),
                    NumberLiteral::I16(i) => i.to_string(),
                    NumberLiteral::U8(i) => i.to_string(),
                    NumberLiteral::I8(i) => i.to_string(),
                    NumberLiteral::ISize(i) => i.to_string(),
                },
        ),
        ExprKind::FunctionCall(fc) => StringTreeNode::with_child_nodes(
            fc.func.0.clone() + " -> " + &format!("{:?}", fc.ret_type),
            fc.args.iter().map(expr),
        ),
        ExprKind::BoolLiteral(b) => {
            StringTreeNode::new("BoolLiteral = ".to_string() + &b.to_string())
        }

        ExprKind::PrimitiveIntComparation(l, r, kind) => {
            let name = match kind {
                crate::semtree::ComparationKind::LesserThan => "<",
                crate::semtree::ComparationKind::LesserEqual => "<=",
                crate::semtree::ComparationKind::GreaterThan => ">",
                crate::semtree::ComparationKind::GreaterEqual => ">=",
                crate::semtree::ComparationKind::Equal => "=",
                crate::semtree::ComparationKind::NotEqual => "!=",
            };
            StringTreeNode::with_child_nodes(
                format!("Binary op {}", name),
                vec![expr(l), expr(r)].into_iter(),
            )
        }
        ExprKind::PrimitiveIntBinOp(l, r, kind) => {
            let name = match kind {
                crate::semtree::IntBinOp::Add => "+",
                crate::semtree::IntBinOp::Substract => "-",
                crate::semtree::IntBinOp::Multiplication => "*",
                crate::semtree::IntBinOp::Division => "/",
                crate::semtree::IntBinOp::Modulo => "mod",
                crate::semtree::IntBinOp::Or => "or",
                crate::semtree::IntBinOp::And => "and",
                crate::semtree::IntBinOp::Xor => "xor",
                crate::semtree::IntBinOp::Shr => "shr",
                crate::semtree::IntBinOp::Shl => "shl",
            };
            StringTreeNode::with_child_nodes(
                format!("Binary op {}", name),
                vec![expr(l), expr(r)].into_iter(),
            )
        }
        ExprKind::PrimitiveIntUnaryOp(_, _) => todo!(),
        ExprKind::BoolBinOp(_, _, _) => todo!(),
        ExprKind::BoolUnaryOp(_, _) => todo!(),
        ExprKind::FloatLiteral(f) => StringTreeNode::new(
            "FloatLiteral = ".to_string()
                + &match &f {
                    crate::semtree::FloatLiteral::F32(t) => t.to_string(),
                    crate::semtree::FloatLiteral::F64(t) => t.to_string(),
                },
        ),
        ExprKind::CharLiteral(c) => StringTreeNode::new(format!("CharLiteral = \"{}\"", c)),
        ExprKind::Deref(dt) => {
            StringTreeNode::with_child_nodes("Defer of ".to_owned(), vec![expr(dt)].into_iter())
        }
        ExprKind::GetElementRefInReffedArray(arr, index) => StringTreeNode::with_child_nodes(
            "Indexation in referenced array ".to_owned(),
            vec![expr(arr), expr(index)].into_iter(),
        ),
        ExprKind::GetLocalVariableRef(lv) => StringTreeNode::with_child_nodes(
            "Ref of local variable ".to_owned(),
            vec![StringTreeNode::new("Variable: ".to_string() + &lv.0)].into_iter(),
        ),
        ExprKind::ConstructRecordFromArgList(x) => StringTreeNode::with_child_nodes(
            format!("New of type: {:?}", expression.ret_type), x.iter().map(expr)),
        ExprKind::GetElementRefInReffedRecord(e, field_num) => StringTreeNode::with_child_nodes(
            format!("Taking ref of {field_num} field in referenced array "),
            vec![expr(e)].into_iter()),
        ExprKind::Clone(_) => todo!(),
        ExprKind::BuildInCall(fc) => StringTreeNode::with_child_nodes(
            fc.func.0.clone() + " -> " + &format!("{:?}", fc.ret_type),
            fc.args.iter().map(expr),
        ),
        ExprKind::Default => todo!(),
        ExprKind::IsNull(e) => StringTreeNode::with_child_nodes(
            format!("IsNull"),
            vec![expr(e)].into_iter()),
        ExprKind::RefCountDecrease(e) => StringTreeNode::with_child_nodes(
            format!("RefCountDecrease"),
            vec![expr(e)].into_iter()),
        ExprKind::RefCountIncrease(e) => StringTreeNode::with_child_nodes(
            format!("RefCountIncrease"),
            vec![expr(e)].into_iter()),
        ExprKind::GetElementBehindReffedReferenceCounter(e) => StringTreeNode::with_child_nodes(
            format!("Taking ref of element behind referenced refcounter "),
            vec![expr(e)].into_iter()),
        ExprKind::ConstructRefcounterFromInternalContent(_) => todo!(),
        ExprKind::FunctionArg(n) => StringTreeNode::new(format!("n-th function arg: {n}")),
    }
}
