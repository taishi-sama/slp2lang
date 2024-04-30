pub mod type_representations;

use std::io;

use console::Term;

pub fn ftest() {

}
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

