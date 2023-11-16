pub enum SLPType {
    PrimitiveType(SLPPrimitiveType),
    Array(Box<SLPType>),
    Struct(StructType),

}
pub enum SLPPrimitiveType {
    
}
pub struct StructType {
    pub name: String,
    pub fields: Vec<(String, SLPType)>,
}