use crate::ast::Type;

pub enum SLPType {
    PrimitiveType(SLPPrimitiveType),
    Pointer(Box<SLPType>),
    DynArray(Box<SLPType>),
    FixedArray{
        begin: i64,
        end: i64,
        ty: Box<SLPType>,
    },
    Struct(StructType),

}
pub enum SLPPrimitiveType {
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    ISize,
    USize,
    String
}
pub struct StructType {
    pub name: String,
    pub fields: Vec<(String, SLPType)>,
}

impl SLPType {
    pub fn from_ast_type(ty: &Type) -> Self {
        match ty {
            Type::Primitive(t) => Self::PrimitiveType(match &t[..] {
                "int8" => SLPPrimitiveType::Int8,
                "int16" => SLPPrimitiveType::Int16,
                "int32" => SLPPrimitiveType::Int32,
                "int64" => SLPPrimitiveType::Int64,
                "isize" => SLPPrimitiveType::ISize,
                "uint8" => SLPPrimitiveType::Uint8,
                "uint16" => SLPPrimitiveType::Uint16,
                "uint32" => SLPPrimitiveType::Uint32,
                "uint64" => SLPPrimitiveType::Uint64,
                "usize" => SLPPrimitiveType::USize,
                "string" => SLPPrimitiveType::String,
                
                _ => panic!("Unknown type!") // TODO: Type alias resolving, structure name resolving
            }),
            Type::Pointer(t) => Self::Pointer(Box::new(Self::from_ast_type(&t))),
            Type::DynArray(t) => Self::DynArray(Box::new(Self::from_ast_type(&t))),
            Type::FixedArray(b, e, t) => Self::FixedArray { begin: *b, end: *e, ty: Box::new(Self::from_ast_type(&t)) },
        }
    }
}