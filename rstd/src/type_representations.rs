use std::{borrow::BorrowMut, cell::UnsafeCell, ptr, sync::atomic::AtomicUsize};



#[repr(C)]
struct SLPRefCounter<T> {
    counter: *const AtomicUsize,
    content: *const UnsafeCell<T>
}

#[repr(C)]
struct SLPDynArray<T> {
    len: usize,
    array: *const UnsafeCell<T>
}
impl<T> SLPDynArray<T> {
    pub unsafe fn into_slice(&self) -> *const [T] {
        unsafe {
            ptr::slice_from_raw_parts((*self.array).get(), self.len)
        }
    }
    pub unsafe fn into_mut_slice(&mut self) -> *mut [T] {
        unsafe {
            let m = (*self.array.cast_mut()).get();
            ptr::slice_from_raw_parts_mut(m, self.len)
        }
    }
}