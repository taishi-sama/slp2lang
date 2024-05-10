use std::{
    ffi::OsString, path::{Path, PathBuf}, process::Command
};

pub trait Linker {
    fn set_target_string(&mut self, target: &str);
    fn get_target_string(&self) -> String;
    fn set_std_find_location(&mut self, path: &Path);
    fn add_target_obj(&mut self, path: &Path);
    fn set_output_file(&mut self, path: &Path);
    fn link(&self) -> bool;
}
#[derive(Debug, Clone)]
pub struct GnuLinker {
    target: String, 
    std_dir: PathBuf,
    std_filename: PathBuf,
    linker: PathBuf, 
    target_objs: Vec<PathBuf>, 
    linker_options: Vec<String>,
    output_path: PathBuf,
}
impl GnuLinker {
    pub fn new_linux_x86_64() -> Self {
        let t = std::env::current_exe().unwrap();
        let dirname = t.parent().unwrap();
        
        Self { 
            target: "x86_64-pc-linux-gnu".into(), 
            linker: "cc".into(), 
            target_objs: vec![], 
            linker_options: vec![],
            
            std_dir: dirname.join("std"),
            std_filename: "librstd.a".into(), 
            output_path: "./a.out".into()
        }
    }
}
impl Linker for GnuLinker {
    fn set_target_string(&mut self, target: &str) {
        self.target = target.into();
    }

    fn get_target_string(&self) -> String {
        self.target.clone()
    }

    fn set_std_find_location(&mut self, path: &Path) {
        self.std_dir = path.into()
    }

    fn add_target_obj(&mut self, path: &Path) {
        self.target_objs.push(path.into())
    }

    fn set_output_file(&mut self, path: &Path) {
        self.output_path = path.into();
    }

    fn link(&self) -> bool {
        let mut comm = Command::new(&self.linker);
        for f in &self.target_objs {
            comm.arg(f);
        }
        comm.arg(self.std_dir.join(&self.std_filename));
        for o in &self.linker_options {
            comm.arg(o);
        }
        comm.arg("-o").arg(&self.output_path);
        let mut linker = comm.spawn().unwrap();
        let res = linker.wait().unwrap();
        if res.code().unwrap() != 0 {
            panic!("Linker error!")
        }
        true
    }
}

#[derive(Debug, Clone)]
pub struct MSVCLinker {
    target: String, 
    std_dir: PathBuf,
    std_filename: PathBuf,
    target_objs: Vec<PathBuf>, 
    linker_options: Vec<String>,
    output_path: PathBuf,
}
impl MSVCLinker {
    fn check_and_get_linker(target: &str) -> Command {
        cc::windows_registry::find(target, "link.exe").unwrap()
    }
    pub fn new_windows_x86_64_msvs() -> Self {
        let target: String = "x86_64-pc-windows-msvc".into();
        let t = std::env::current_exe().unwrap();
        Self::check_and_get_linker(&target);
        let dirname = t.parent().unwrap();
        
        Self { 
            target, 
            target_objs: vec![], 
            linker_options: vec![],
            
            std_dir: dirname.join("std"),
            std_filename: "rstd.lib".into(), 
            output_path: "./a.exe".into()
        }
    }
}
impl Linker for MSVCLinker {
    fn set_target_string(&mut self, target: &str) {
        self.target = target.into();
    }

    fn get_target_string(&self) -> String {
        self.target.clone()
    }

    fn set_std_find_location(&mut self, path: &Path) {
        self.std_dir = path.into()
    }

    fn add_target_obj(&mut self, path: &Path) {
        self.target_objs.push(path.into())
    }

    fn set_output_file(&mut self, path: &Path) {
        self.output_path = path.into();
    }

    fn link(&self) -> bool {
        let mut comm = Self::check_and_get_linker(&self.target);
        //let mut comm = Command::new(&self.linker);
        for f in &self.target_objs {
            comm.arg(f);
        }
        comm.arg(self.std_dir.join(&self.std_filename));
        for o in &self.linker_options {
            comm.arg(o);
        }
        let mut output: OsString = "-out:".into();
        output.push(&self.output_path);
        comm.arg(output);

        comm.arg("-nologo");
        comm.arg("ws2_32.lib");
        comm.arg("kernel32.lib");
        comm.arg("ntdll.lib");
        comm.arg("synchronization.lib");
        comm.arg("shell32.lib");
        comm.arg("psapi.lib");
        comm.arg("userenv.lib");
        comm.arg("-defaultlib:libcmt");
        comm.arg("-defaultlib:oldnames");

        let mut linker = comm.spawn().unwrap();
        let res = linker.wait().unwrap();
        if res.code().unwrap() != 0 {
            panic!("Linker error!")
        }
        true
    }
}

