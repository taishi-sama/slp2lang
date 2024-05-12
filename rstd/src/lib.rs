pub mod type_representations;


use std::{borrow::Borrow, io::Write};

use console::Term;
use type_representations::{SLPDynArray, SLPRefCounter};

pub fn ftest() {}
#[no_mangle]
pub extern "C" fn rc_putchar(c: i32) {
    if let Some(t) = char::from_u32(c as _) {
        print!("{t}")
    }
}
#[no_mangle]
pub extern "C" fn rc_putint32(c: i32) {
    print!("{c}")
}
#[no_mangle]
pub extern "C" fn rc_putint64(c: i64) {
    print!("{c}")
}
#[no_mangle]
pub extern "C" fn rc_getchar() -> u32 {
    Term::stdout().read_char().unwrap() as u32
}
#[no_mangle]
pub extern "C" fn rc_putstring(arg: *const SLPRefCounter<SLPDynArray<u32>>) {
    if let Some(arg) = unsafe { arg.as_ref() } {
        if let Some(a) = unsafe { arg.content.as_ref() } {
            let r = unsafe { (a.get() as *const SLPDynArray<u32>).as_ref().unwrap() };
            let l = r.len;
            for i in 0..l {
                let t = unsafe { r.array.add(i) };
                let l = unsafe { t.as_ref().unwrap() };
                let number = unsafe { l.get().read() };
                //println!("reading some char");
                if let Some(c) = char::from_u32(number) {
                    //println!("{number}")
                    print!("{c}");
                    
                }
            }
            std::io::stdout().flush().unwrap();
        }
    }
}