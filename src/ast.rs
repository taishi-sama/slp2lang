#[derive(Debug, Clone, Copy)]
pub struct Loc{
    pub begin: usize,
    pub end: usize,
}
impl Loc {
    pub fn new(begin: usize, end: usize) -> Self {Self{begin, end}}
}
#[derive(Debug, Clone)]
pub struct ProgramFile {
    pub declarations: Vec<Declaration>
}
#[derive(Debug, Clone)]
pub enum Declaration {
    Function(FunctionBody),
    ExternFunction(ExternFunctionBody)
}
#[derive(Debug, Clone)]
pub struct FunctionBody {
    pub function_name:String,
    pub function_args:Vec<ArgDecl>,
    pub return_arg: TypeDecl,
    pub body: StatementBlock,
    pub loc: Loc,
}
#[derive(Debug, Clone)]
pub struct ExternFunctionBody {
    pub function_name:String,
    pub function_args:ArgDeclList,
    pub return_arg: Type,
    pub loc: Loc,
}
#[derive(Debug, Clone)]
pub enum Expr {
    Constant(Loc, Constant),
    Ident(Loc, String),
    OpBinPlus(Loc, Box<Expr>, Box<Expr>),
    OpBinMinus(Loc, Box<Expr>, Box<Expr>),
    //Multiplication
    OpBinAsterisk(Loc, Box<Expr>, Box<Expr>),
    //Division
    OpBinSlash(Loc, Box<Expr>, Box<Expr>),
    OpBinDiv(Loc, Box<Expr>, Box<Expr>),
    OpBinMod(Loc, Box<Expr>, Box<Expr>),

    OpUnPlus(Loc, Box<Expr>),
    OpUnMinus(Loc, Box<Expr>),

    OpBinAnd(Loc, Box<Expr>, Box<Expr>),
    OpBinOr(Loc, Box<Expr>, Box<Expr>),
    OpBinXor(Loc, Box<Expr>, Box<Expr>),
    OpUnNot(Loc, Box<Expr>),
    OpBinShl(Loc, Box<Expr>, Box<Expr>),
    OpBinShr(Loc, Box<Expr>, Box<Expr>),

    OpBinLesser(Loc, Box<Expr>, Box<Expr>),
    OpBinGreater(Loc, Box<Expr>, Box<Expr>),
    OpBinLesserEq(Loc, Box<Expr>, Box<Expr>),
    OpBinGreaterEq(Loc, Box<Expr>, Box<Expr>),
    OpBinEq(Loc, Box<Expr>, Box<Expr>),
    OpBinNotEq(Loc, Box<Expr>, Box<Expr>),
    
    OpUnDeref(Loc, Box<Expr>),
    OpUnGetRef(Loc, Box<Expr>),
    
    //OpUnTypecast(Loc, TypeDecl, Box<Expr>),
    //Distinguish from typecast at next stages
    OpFunctionCall(Loc, Box<Expr>, Vec<Expr>),
    OpUnAs(Loc, Box<Expr>, TypeDecl),
    //Can be just namespace specification or proper method call 
    OpDot(Loc, Box<Expr>, String),
    OpNew(Loc, Type, Vec<Expr>)
}
#[derive(Debug, Clone)]
pub enum Constant {
    String(String),
    Int64(i64),
    Float64(f64),
    Bool(bool)
}
pub type StatementBlock = Vec<Statement>; 
#[derive(Debug, Clone)]
pub enum Statement {
    CodeBlock(Loc, Vec<Statement>),
    Print(Loc, Box<Expr>),
    //RHS, LHS
    Assignment(Loc, Box<Expr>, Box<Expr>),
    If(Loc, Box<Expr>, Box<Statement>, Option<Box<Statement>>),
    While(Loc, Box<Expr>, Box<Statement>),
    RepeatUntil(Loc, Box<Expr>, Box<Statement>),
    VarDecl(Loc, VarDecl),
    Empty()
}
#[derive(Debug, Clone)]
pub enum VarDecl{
    Multiple(Vec<String>, TypeDecl),
    ExplicitType(String, TypeDecl, Box<Expr>),
    ImplicitType(String, Box<Expr>),
}
#[derive(Debug, Clone)]
pub struct ArgDecl {
    pub names: Vec<String>,
    pub ty: TypeDecl,
    pub loc: Loc,
}
pub type ArgDeclList = Vec<ArgDecl>;

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub loc: Loc,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum Type {
    Primitive(String),
    Pointer(Box<Type>),
    //MultiDim arrays aren't supported yet
    DynArray(Box<Type>),
    //Only integer indexes are supported yet
    FixedArray(i64, i64, Box<Type>)
}
impl Type {
    pub fn void() -> Self {Type::Primitive("void".to_string())}
}