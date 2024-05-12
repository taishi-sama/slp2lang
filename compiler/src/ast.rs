use std::fmt::Display;

use crate::compiler::FileId;

#[derive(Debug, Clone, Copy)]
pub struct Loc {
    pub begin: usize,
    pub end: usize,
    pub fid: FileId,
}

impl Loc {
    pub fn new(begin: usize, end: usize, fid: FileId) -> Self {
        Self { begin, end, fid }
    }
}
impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Loc[{}..{}]", self.begin, self.end)
    }
}
#[derive(Debug, Clone)]
pub struct ProgramFile {
    pub uses: Vec<Usings>,
    pub declarations: Vec<Declaration>,
}
#[derive(Debug, Clone)]
pub enum Usings {
    Name(Loc, String),
    Path(Loc, String),
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Function(FunctionBody),
    ExternFunction(ExternFunctionBody),
    TypeDeclSection(TypeDeclSectionBody),
}

#[derive(Debug, Clone)]
pub struct TypeDeclSectionBody {
    pub decls: Vec<TypeDeclElement>,
}
#[derive(Debug, Clone)]
pub enum TypeDeclElement {
    TypeAlias(Loc, String, Type),
    RecordDeclare(Loc, String, Vec<RecordField>, RecordType),
}
#[derive(Debug, Clone)]
pub enum RecordType {
    Record,
    Class,
}
#[derive(Debug, Clone)]
pub struct RecordField {
    pub loc: Loc,
    pub id: String,
    pub ty: TypeDecl,
}

#[derive(Debug, Clone)]
pub struct FunctionBody {
    pub function_name: String,
    pub function_args: Vec<ArgDecl>,
    pub return_arg: TypeDecl,
    pub body: StatementBlock,
    pub loc: Loc,
}
#[derive(Debug, Clone)]
pub struct ExternFunctionBody {
    pub function_name: String,
    pub function_args: ArgDeclList,
    pub return_arg: TypeDecl,
    pub loc: Loc,
}
#[derive(Debug, Clone)]
pub struct Identificator {
    pub name: String,
    pub path: Vec<String>,
}
impl Display for Identificator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in &self.path {
            write!(f, "{i}::")?;
        }
        write!(f, "{}", self.name)
    }
}
#[derive(Debug, Clone)]
pub enum Expr {
    Constant(Loc, Constant),
    Ident(Loc, Identificator),
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
    OpBinIndex(Loc, Box<Expr>, Box<Expr>),
    //Distinguish from typecast at next stages
    OpFunctionCall(Loc, FunctionCall),
    OpUnAs(Loc, Box<Expr>, TypeDecl),

    OpMethodCall(Loc, Box<Expr>, String),
    OpNew(Loc, Type, Option<Box<Expr>>, Vec<Expr>),
    NilLiteral(Loc),
}

#[derive(Debug, Clone)]
pub enum Constant {
    String(String),
    Int(String),
    Float(String),
    Char(String),
    Bool(bool),
}
pub type StatementBlock = Vec<Statement>;
#[derive(Debug, Clone)]
pub enum Statement {
    CodeBlock(Loc, StatementBlock),
    FunctionCall(Loc, FunctionCall),
    //RHS, LHS
    Assignment(Loc, Box<Expr>, Box<Expr>),
    If(Loc, Box<Expr>, Box<Statement>, Option<Box<Statement>>),
    While(Loc, Box<Expr>, Box<Statement>),
    For(Loc, ForLoop),
    RepeatUntil(Loc, Box<Expr>, Box<Statement>),
    VarDecl(Loc, VarDecl),
    Defer(Loc, Box<Statement>),

    Empty(),
}
#[derive(Debug, Clone)]
pub struct ForLoop {
    pub is_new: bool,
    pub var_id: String,
    pub initial_value: Box<Expr>,
    pub direction: ForDirection,
    pub final_value: Box<Expr>,
    pub body: Box<Statement>,
}
#[derive(Debug, Clone)]
pub enum ForDirection {
    Up,
    Down,
}

#[derive(Debug, Clone)]
pub enum VarDecl {
    Multiple(Vec<String>, TypeDecl),
    ExplicitType(String, TypeDecl, Box<Expr>),
    ImplicitType(String, Box<Expr>),
}
#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
}
#[derive(Debug, Clone)]
pub struct ArgDecl {
    pub names: Vec<String>,
    pub var_param: bool,
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
    Primitive(Identificator),
    Pointer(Box<Type>),
    //MultiDim arrays aren't supported yet
    DynArray(Box<Type>),
    //Only integer indexes are supported yet
    FixedArray(i64, i64, Box<Type>),
}
impl Type {
    pub fn void() -> Self {
        Type::Primitive(Identificator {
            name: "void".to_string(),
            path: vec![],
        })
    }
}