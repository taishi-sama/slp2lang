use std::{
    io::Error,
    path::{Path, PathBuf},
    process::Command,
};

#[derive(Debug, Clone)]
pub struct LinkerBuilder {
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
    pub fn new_linux_x86_64<'a, 'b>() -> Self {
        Self {
            linker: "cc".into(),
            system_dyn_linker: None,
            system_crt: vec![
                "./bin_blobs/librstd.a".into()
            ],
            libc_options: vec!["-lc".to_string()],
            linker_options: vec![],
        }
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
