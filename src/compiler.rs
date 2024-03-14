// Do syntax analysys of initial file
// Take usages
// Do syntax analysys of usages recursive
// Build symbol table of usages
// Combine tables
// Compile all depedencies using cross-combined symbol tables
// Link them using LLVM internal linker
// Link resulting .obj file into executable

use std::{collections::{HashMap, VecDeque}, fs, path::{Path, PathBuf}, sync::Arc};

use anyhow::Ok;

use crate::ast::{self, ProgramFile};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct FileId(pub u32);

pub struct Compiler{
    filename_to_id: HashMap<String, FileId>,
    asts: HashMap<FileId, Arc<ProgramFile>>,
    deps: HashMap<FileId, Vec<FileId>>,
    queue_on_check: VecDeque<FileId>
}

impl Compiler {
    pub fn new() -> Self {
        Self{filename_to_id: Default::default(), asts: Default::default(), deps: Default::default(), queue_on_check: Default::default()}
    }
    pub fn start_compilation(&mut self, includes: Vec<PathBuf>, initial_file: PathBuf) -> anyhow::Result<()> {
        let text = fs::read_to_string(&initial_file)?;
        let ast = crate::grammar::ProgramBlockParser::new().parse(&text).unwrap();
        
        let fid = self.path_to_fileid(&initial_file);
        Ok(())
    }
    fn path_to_fileid(&mut self, path: &Path) -> FileId {
        //Properly do hierarchy
        let p = path.file_stem().unwrap().to_string_lossy().into_owned();
        if let Some(f) = self.filename_to_id.get(&p) {
            f.clone()
        }
        else {
            let counter = self.filename_to_id.len() as u32;
            self.filename_to_id.insert(p, FileId(counter));
            FileId(counter)
        }
    }
}