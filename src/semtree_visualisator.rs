use std::iter;

use text_trees::StringTreeNode;

use crate::semtree::{ProgramRoot, Function, ExternFunction};


pub fn get_program_root(root: &ProgramRoot) -> StringTreeNode {
    StringTreeNode::with_child_nodes("root".into(), root.funcs.iter().map(function).chain(root.extern_funcs.iter().map(extern_function)))
}
pub fn function(func: &Function) -> StringTreeNode {
    StringTreeNode::new("function ".to_owned() + &func.function_name.0)
}
pub fn extern_function(func: &ExternFunction) -> StringTreeNode {
    StringTreeNode::new("function ".to_owned() + &func.function_name.0)
}