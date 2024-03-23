use std::sync::Arc;

use crate::{ast::Type, errors::SemTreeBuildErrors};
#[derive(Debug, Clone, PartialEq)]
pub enum SLPType {
    PrimitiveType(SLPPrimitiveType),
    Pointer(Box<SLPType>),
    DynArray(Box<SLPType>),
    FixedArray {
        begin: i64,
        end: i64,
        ty: Box<SLPType>,
    },
    Struct(Arc<StructType>),
}
#[derive(Debug, Clone, PartialEq)]
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
    String,
    Bool,
    Void,
}
impl SLPPrimitiveType {
    pub fn is_int(&self) -> bool {
        match self {
            SLPPrimitiveType::Int8 | SLPPrimitiveType::Int16 | SLPPrimitiveType::Int32 | SLPPrimitiveType::Int64 => true,
            SLPPrimitiveType::Uint8 | SLPPrimitiveType::Uint16 | SLPPrimitiveType::Uint32 | SLPPrimitiveType::Uint64 => true,


            SLPPrimitiveType::ISize => true,
            SLPPrimitiveType::USize => true,
            SLPPrimitiveType::String => false,
            SLPPrimitiveType::Bool => false,
            SLPPrimitiveType::Void => false,
        }
    }
    pub fn is_unsigned_int(&self) -> bool {
        if self.is_int() {
            match self {
                SLPPrimitiveType::Int8 | SLPPrimitiveType::Int16 | SLPPrimitiveType::Int32 | SLPPrimitiveType::Int64 => false,
                SLPPrimitiveType::Uint8 | SLPPrimitiveType::Uint16 | SLPPrimitiveType::Uint32 | SLPPrimitiveType::Uint64 => true,


                SLPPrimitiveType::ISize => false,
                SLPPrimitiveType::USize => true,
                _ => unreachable!()
            }
        }
        else {false}
    }
    
}
#[derive(Debug, Clone, PartialEq)]
pub struct StructType {
    pub name: String,
    pub fields: Vec<(String, SLPType)>,
}

impl SLPType {
    pub fn is_int(&self) -> bool {
        if let SLPType::PrimitiveType(p) = self {
            p.is_int()
        }
        else {
            false
        }
    }
    pub fn is_unsigned_int(&self) -> bool {
        if let SLPType::PrimitiveType(p) = self {
            p.is_unsigned_int()
        }
        else {
            false
        }
    }
    pub fn is_primitive_int_comparable(l: &Self, r: &Self) -> Option<SLPType> {
        if l == r {
            if l.is_int() {
                Some(l.clone())
            }
            else {
                todo!()
            }
        }
        else {
            todo!()
        }
    }
    pub fn is_bool(&self) -> bool {
        if let &SLPType::PrimitiveType(SLPPrimitiveType::Bool) = self {
            true
        }
        else { 
            false
        }
    }
    pub fn is_void(&self) -> bool {
        if let &SLPType::PrimitiveType(SLPPrimitiveType::Void) = self {
            true
        }
        else { 
            false
        }
    }
    pub fn from_ast_type(ty: &Type) -> Result<Self, SemTreeBuildErrors> {
        match ty {
            Type::Primitive(t) => if t.path.is_empty() { Ok(Self::PrimitiveType(match &t.name[..] {
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
                "bool" => SLPPrimitiveType::Bool,
                "void" => SLPPrimitiveType::Void,
                _ => panic!("Unknown type!"), // TODO: Type alias resolving, structure name resolving
            })) } else {todo!()},
            Type::Pointer(t) => Ok(Self::Pointer(Box::new(Self::from_ast_type(&t)?))),
            Type::DynArray(t) => Ok(Self::DynArray(Box::new(Self::from_ast_type(&t)?))),
            Type::FixedArray(b, e, t) => Ok(Self::FixedArray {
                begin: *b,
                end: *e,
                ty: Box::new(Self::from_ast_type(&t)?),
            }),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct TypeTable {

}