
//Declarations before any typechecking or name resolving, to make typechecks or name resolvings possible
pub struct RawDeclarations {
    decls: Vec<RawDeclaration>

}
pub enum RawDeclaration {
    FunctionDecl()
}
pub struct ProgramFileTarget {
    decls: Vec<Declaration>
}
pub enum Declaration {
    
}