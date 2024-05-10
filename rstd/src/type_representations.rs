use std::{cell::UnsafeCell, ptr, sync::atomic::AtomicUsize};
#[allow(dead_code)]
#[repr(C)]
struct SLPRefCounter<T> {
    counter: *const AtomicUsize,
    content: *const UnsafeCell<T>,
}
#[allow(dead_code)]
#[repr(C)]
struct SLPDynArray<T> {
    len: usize,
    array: *const UnsafeCell<T>,
}
impl<T> SLPDynArray<T> {
    #[allow(dead_code)]
    pub unsafe fn into_slice(&self) -> *const [T] {
        unsafe { ptr::slice_from_raw_parts((*self.array).get(), self.len) }
    }
    #[allow(dead_code)]
    pub unsafe fn into_mut_slice(&mut self) -> *mut [T] {
        unsafe {
            let m = (*self.array.cast_mut()).get();
            ptr::slice_from_raw_parts_mut(m, self.len)
        }
    }
}
