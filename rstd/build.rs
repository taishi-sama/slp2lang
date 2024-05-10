use std::{env, fs, path::{Path, PathBuf}};

fn main() {

    println!("cargo::rerun-if-changed=./std");
    println!("cargo::rerun-if-changed=build.rs");
    let dir = fs::read_dir("../std").unwrap();
    let std = dir.filter_map(|x|{
        let f = x.as_ref().unwrap().path();
        if f.is_file() {
            Some(f)
        } else {
            None
        }
    });
    let out_dir: PathBuf = env::var("OUT_DIR").unwrap().into();
    let std_dir = out_dir.join("std");
    fs::create_dir_all(&std_dir).unwrap();
    for f in std {
        let filename = f.file_name().unwrap();
        fs::copy(&f, std_dir.join(filename)).unwrap();
    }
}