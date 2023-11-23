pub enum SLPType {
    PrimitiveType(SLPPrimitiveType),
    Array(Box<SLPType>),
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
    String
}
pub struct StructType {
    pub name: String,
    pub fields: Vec<(String, SLPType)>,
}