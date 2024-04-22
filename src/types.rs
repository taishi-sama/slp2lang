use crate::{compiler::FileId, symbols::Id};


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SLPType {
    PrimitiveType(SLPPrimitiveType),
    Pointer(Box<SLPType>),
    AutoderefPointer(Box<SLPType>),
    DynArray(Box<SLPType>),
    FixedArray {
        size: u64,
        index_offset: i64,
        ty: Box<SLPType>,
    },
    ///Bool indicates that struct is trivially copiable
    Struct(String, Id, bool), 
    RefCounter(Box<SLPType>),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    Float32,
    Float64,
    String,
    StringLiteral(u32),
    Char,
    Bool,
    Void,
}
impl SLPPrimitiveType {
    pub fn is_int(&self) -> bool {
        match self {
            SLPPrimitiveType::Int8
            | SLPPrimitiveType::Int16
            | SLPPrimitiveType::Int32
            | SLPPrimitiveType::Int64 => true,
            SLPPrimitiveType::Uint8
            | SLPPrimitiveType::Uint16
            | SLPPrimitiveType::Uint32
            | SLPPrimitiveType::Uint64 => true,

            SLPPrimitiveType::ISize => true,
            SLPPrimitiveType::USize => true,
            
            SLPPrimitiveType::String => false,
            SLPPrimitiveType::Char => false,
            SLPPrimitiveType::Bool => false,
            SLPPrimitiveType::Void => false,
            SLPPrimitiveType::Float32 => false,
            SLPPrimitiveType::Float64 => false,
            SLPPrimitiveType::StringLiteral(_) => false,
        }
    }
    //Size in bytes
    pub fn get_number_size(&self) -> Option<u8> {
        match self {
            SLPPrimitiveType::Int8 => Some(1),
            SLPPrimitiveType::Int16 => Some(2),
            SLPPrimitiveType::Int32 => Some(4),
            SLPPrimitiveType::Int64 => Some(8),
            SLPPrimitiveType::Uint8 => Some(1),
            SLPPrimitiveType::Uint16 => Some(2),
            SLPPrimitiveType::Uint32 => Some(4),
            SLPPrimitiveType::Uint64 => Some(8),
            SLPPrimitiveType::ISize => todo!(),
            SLPPrimitiveType::USize => todo!(),
            SLPPrimitiveType::Float32 => Some(4),
            SLPPrimitiveType::Float64 => Some(8),
            SLPPrimitiveType::String => None,
            SLPPrimitiveType::Bool => None,
            SLPPrimitiveType::Void => None,
            SLPPrimitiveType::Char => Some(4),
            SLPPrimitiveType::StringLiteral(_) => None,
        }
    }
    pub fn is_unsigned_int(&self) -> bool {
        if self.is_int() {
            match self {
                SLPPrimitiveType::Int8
                | SLPPrimitiveType::Int16
                | SLPPrimitiveType::Int32
                | SLPPrimitiveType::Int64 => false,
                SLPPrimitiveType::Uint8
                | SLPPrimitiveType::Uint16
                | SLPPrimitiveType::Uint32
                | SLPPrimitiveType::Uint64 => true,

                SLPPrimitiveType::ISize => false,
                SLPPrimitiveType::USize => true,
                _ => unreachable!(),
            }
        } else {
            false
        }
    }

}
#[derive(Debug, Clone, PartialEq)]
pub struct StructType {
    pub is_class: bool,
    pub is_copiable: bool,
    pub name: Id,
    pub fields: Vec<(Id, SLPType)>,
}

impl SLPType {
    pub fn bool() -> Self {
        SLPType::PrimitiveType(SLPPrimitiveType::Bool)
    }
    pub fn isize() -> Self {
        SLPType::PrimitiveType(SLPPrimitiveType::ISize)
    }
    pub fn void() -> Self {
        SLPType::PrimitiveType(SLPPrimitiveType::Void)
    }
    pub fn wrap_autoderef_or_pass(&self) -> Self {
        if let SLPType::AutoderefPointer(_) = self {
            self.clone()
        } else {
            SLPType::AutoderefPointer(Box::new(self.clone()))
        }
    }
    pub fn normalized_name(&self) -> Id {
        match &self {
            SLPType::PrimitiveType(ty) => match ty {
                SLPPrimitiveType::Int8 => Id("int8".to_string()),
                SLPPrimitiveType::Int16 => Id("int16".to_string()),
                SLPPrimitiveType::Int32 => Id("int32".to_string()),
                SLPPrimitiveType::Int64 => Id("int64".to_string()),
                SLPPrimitiveType::Uint8 => Id("uint8".to_string()),
                SLPPrimitiveType::Uint16 => Id("uint16".to_string()),
                SLPPrimitiveType::Uint32 => Id("uint32".to_string()),
                SLPPrimitiveType::Uint64 => Id("uint64".to_string()),
                SLPPrimitiveType::ISize => Id("isize".to_string()),
                SLPPrimitiveType::USize => Id("usize".to_string()),
                SLPPrimitiveType::Float32 => Id("float32".to_string()),
                SLPPrimitiveType::Float64 => Id("float64".to_string()),
                SLPPrimitiveType::String => Id("string".to_string()),
                SLPPrimitiveType::StringLiteral(l) => Id(format!("strlit{}", l)),
                SLPPrimitiveType::Char => Id(format!("char")),
                SLPPrimitiveType::Bool => Id(format!("bool")),
                SLPPrimitiveType::Void => todo!(),
            },
            SLPType::Pointer(t) => Id(format!("ptr@{}", t.normalized_name().0)),
            SLPType::AutoderefPointer(t) => Id(format!("autoref@{}", t.normalized_name().0)),
            SLPType::DynArray(t) => Id(format!("dinarray@{}", t.normalized_name().0)),
            SLPType::FixedArray { size, index_offset, ty } => Id(format!("fixsizearray{size}_{index_offset}@{}", ty.normalized_name().0)),
            SLPType::Struct(s, t, _) => Id(format!("struct_{}_{}",s, &t.0)),
            SLPType::RefCounter(t) => Id(format!("rc@{}", t.normalized_name().0)),
        }
    } 
    pub fn is_trivially_copiable(&self) -> bool {
        match self {
            SLPType::PrimitiveType(x) => match x {
                SLPPrimitiveType::Int8 => true,
                SLPPrimitiveType::Int16 => true,
                SLPPrimitiveType::Int32 => true,
                SLPPrimitiveType::Int64 => true,
                SLPPrimitiveType::Uint8 => true,
                SLPPrimitiveType::Uint16 => true,
                SLPPrimitiveType::Uint32 => true,
                SLPPrimitiveType::Uint64 => true,
                SLPPrimitiveType::ISize => true,
                SLPPrimitiveType::USize => true,
                SLPPrimitiveType::Float32 => true,
                SLPPrimitiveType::Float64 => true,
                SLPPrimitiveType::String => todo!(),
                SLPPrimitiveType::StringLiteral(_) => todo!(),
                SLPPrimitiveType::Char => true,
                SLPPrimitiveType::Bool => true,
                SLPPrimitiveType::Void => todo!(),
            },
            SLPType::Pointer(x) => true,
            SLPType::AutoderefPointer(x) => true,
            SLPType::DynArray(_) => false,
            SLPType::FixedArray { size, index_offset, ty } => ty.is_trivially_copiable(),
            SLPType::Struct(_, _, t) => *t,
            SLPType::RefCounter(_) => false,
        }
    }
    pub fn get_underlying_pointer_type(&self) -> Option<&SLPType> {
        match self {
            SLPType::PrimitiveType(_) => None,
            SLPType::Pointer(ty) => Some(&ty),
            SLPType::DynArray(_d) => None,
            SLPType::FixedArray {
                size: _,
                index_offset: _,
                ty: _,
            } => None,
            SLPType::Struct(_, _, _) => None,
            SLPType::AutoderefPointer(ty) => Some(&ty),
            SLPType::RefCounter(_) => None,
        }
    }
    pub fn get_underlying_autoderef_type(&self) -> Option<&SLPType> {
        match self {
            SLPType::PrimitiveType(_) => None,
            SLPType::Pointer(_ty) => None,
            SLPType::DynArray(_d) => None,
            SLPType::FixedArray {
                size: _,
                index_offset: _,
                ty: _,
            } => None,
            SLPType::Struct(_, _, _) => None,
            SLPType::AutoderefPointer(ty) => Some(&ty),
            SLPType::RefCounter(_) => None,
        }
    }
    pub fn get_underlying_array_type(&self) -> Option<&SLPType> {
        match self {
            SLPType::PrimitiveType(_) => None,
            SLPType::Pointer(_) => None,
            SLPType::DynArray(d) => Some(d.as_ref()),
            SLPType::FixedArray {
                size: _,
                index_offset: _,
                ty,
            } => Some(ty.as_ref()),
            SLPType::Struct(_, _, _) => None,
            SLPType::AutoderefPointer(_) => None,
            SLPType::RefCounter(_) => None,
        }
    }
    pub fn get_underlying_refcounter_type(&self) -> Option<&SLPType> {
        match self {
            SLPType::PrimitiveType(_) => None,
            SLPType::Pointer(_) => None,
            SLPType::DynArray(_) => None,
            SLPType::FixedArray {
                size: _,
                index_offset: _,
                ty,
            } => None,
            SLPType::Struct(_, _, _) => None,
            SLPType::AutoderefPointer(_) => None,
            SLPType::RefCounter(rc) => Some(&rc),
        }
    }
    pub fn is_static_sized_array(&self) -> bool {
        if let SLPType::FixedArray {
            size: _size,
            index_offset: _index_offset,
            ty: _ty,
        } = self
        {
            true
        } else {
            false
        }
    }
    pub fn is_any_int(&self) -> bool {
        if let SLPType::PrimitiveType(p) = self {
            p.is_int()
        } else {
            false
        }
    }
    pub fn is_unsigned_int(&self) -> bool {
        if let SLPType::PrimitiveType(p) = self {
            p.is_unsigned_int()
        } else {
            false
        }
    }
    pub fn is_primitive_int_comparable(l: &Self, r: &Self) -> Option<SLPType> {
        if l == r {
            if l.is_any_int() {
                Some(l.clone())
            } else {
                todo!()
            }
        } else {
            todo!()
        }
    }
    pub fn is_bool(&self) -> bool {
        if let &SLPType::PrimitiveType(SLPPrimitiveType::Bool) = self {
            true
        } else {
            false
        }
    }
    pub fn is_void(&self) -> bool {
        if let &SLPType::PrimitiveType(SLPPrimitiveType::Void) = self {
            true
        } else {
            false
        }
    }
    pub fn is_char(&self) -> bool {
        if let &SLPType::PrimitiveType(SLPPrimitiveType::Char) = self {
            true
        } else {
            false
        }
    }
    pub fn get_number_size(&self) -> Option<u8> {
        if let SLPType::PrimitiveType(pt) = self {
            pt.get_number_size()
        } else {
            None
        }
    }
}
