use std::{
    ffi::{OsStr, OsString}, io::Error, path::{Path, PathBuf}, process::Command
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


#[derive(Debug, Clone)]
pub struct LinkerBuilder {
    target: String, 
    linker: PathBuf,
    system_dyn_linker: Option<PathBuf>,
    system_crt: Vec<PathBuf>,
    libc_options: Vec<String>,
    linker_options: Vec<String>,
}

impl LinkerBuilder {
    pub fn add_linker_option(mut self, option: &str) -> Self {
        self.linker_options.push(option.to_string());
        self
    }
    pub fn new_linux_x86_64() -> Self {
        Self {
            target: "x86_64-pc-linux-gnu".to_string(),
            linker: "cc".into(),
            system_dyn_linker: None,
            system_crt: vec!["./bin_blobs/librstd.a".into()],
            libc_options: vec!["-lc".to_string()],
            linker_options: vec![],
        }
    }
    pub fn new_windows_x86_64_msvc() -> Self {
        todo!()
    }
    pub fn link_gnu_linker_flavor(
        &self,
        link_asan: bool,
        main_linkable_object: impl AsRef<Path>,
        output_object: impl AsRef<Path>,
    ) -> Result<(), Error> {
        let mlo: &Path = main_linkable_object.as_ref();
        let o: &Path = output_object.as_ref();
        let main_linkable_object: PathBuf = mlo.to_path_buf();
        let output_object: PathBuf = o.to_path_buf();

        let mut comm = Command::new(self.linker.as_path());
        if link_asan {
            comm.arg("-lasan");
        }
        comm.arg(main_linkable_object);

        if let Some(sys_linker) = &self.system_dyn_linker {
            comm.arg("-dynamic-linker").arg(sys_linker);
        }
        for crt_o in &self.system_crt {
            comm.arg(crt_o);
        }
        for libc_option in &self.libc_options {
            comm.arg(libc_option);
        }
        for linker_option in &self.linker_options {
            comm.arg(linker_option);
        }
        comm.arg("-o").arg(output_object);

        //println!("{:#?}", comm);

        let mut linker = comm.spawn()?;
        let res = linker.wait()?;
        if res.code().unwrap() != 0 {
            panic!("Linker error!")
        }

        Ok(())
    }
}
