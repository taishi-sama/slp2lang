#[derive(Debug, Clone)]
pub struct Program {
    pub declarations: Vec<Declaration>
}
#[derive(Debug, Clone)]
pub enum Declaration {
    Function(FunctionBody),
}
#[derive(Debug, Clone)]
pub struct FunctionBody {
    pub function_name:String,
    pub body: StatementBlock,
}
#[derive(Debug, Clone)]
pub enum Expr {
    Constant(Constant),
    OpAdd(Box<Expr>, Box<Expr>)
}
#[derive(Debug, Clone)]
pub enum Constant {
    String(String),
    Int64(i64),
    Float64(f64),
}
pub type StatementBlock = Vec<Statement>; 
#[derive(Debug, Clone)]
pub enum Statement {
    CodeBlock(Vec<Statement>),
    Print(Box<Expr>),

}