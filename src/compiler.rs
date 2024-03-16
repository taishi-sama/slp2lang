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
    includes: Vec<PathBuf>,
    filename_to_id: HashMap<String, FileId>,
    asts: HashMap<FileId, Arc<ProgramFile>>,
    deps: HashMap<FileId, Vec<FileId>>,
    queue_on_check: VecDeque<FileId>
}

impl Compiler {
    pub fn new(includes: Vec<PathBuf>) -> Self {
        Self{filename_to_id: Default::default(), asts: Default::default(), deps: Default::default(), queue_on_check: Default::default(), includes}
    }
    pub fn start_compilation(&mut self, initial_file: PathBuf) -> anyhow::Result<()> {
        let text = fs::read_to_string(&initial_file)?;
        let ast = crate::grammar::ProgramBlockParser::new().parse(&text).unwrap();
        
        let fid = self.path_to_fileid(&initial_file);
        let pf = Arc::new(ast);
        self.asts.insert(fid.0, pf.clone());
        self.check_childs(&pf)?;
        self.check_queue()?;
        Ok(())
    }
    fn find_first_applicable_child(&self, filename: &str) -> Option<PathBuf> {
        for i in &self.includes {
            let t = Path::join(&i, format!("{}.slp2", filename));
            if t.is_file() {
                return Some(t);
            }
        }
        None
    }
    fn check_childs(&mut self, f: &ProgramFile) -> anyhow::Result<()> {
        for u in &f.uses {
            match u {
                ast::Usings::Name(_, n) => {
                    println!("Checking dep: {n}");
                    let f = self.find_first_applicable_child(n).unwrap();
                    let (fid, cond) = self.path_to_fileid(&f);
                    if cond {
                        println!("File loaded: {}", f.to_string_lossy());
                        let text = fs::read_to_string(&f)?;
                        let ast = crate::grammar::ProgramBlockParser::new().parse(&text).unwrap();
                        self.asts.insert(fid, Arc::new(ast));
                        self.queue_on_check.push_back(fid);
                    }
                    else {
                        println!("File already added: {}", f.to_string_lossy());

                    }
                    
                },
                ast::Usings::Path(_, _) => todo!(),
            }
        };
        Ok(())
    }
    fn check_queue(&mut self) -> anyhow::Result<()> {
        while let Some(entry) = self.queue_on_check.pop_front() {
            let t = self.asts[&entry].clone();
            self.check_childs(&t)?;
        }
        Ok(())
    }

    fn path_to_fileid(&mut self, path: &Path) -> (FileId, bool) {
        //Properly do hierarchy
        let p = path.file_stem().unwrap().to_string_lossy().into_owned();
        if let Some(f) = self.filename_to_id.get(&p) {
            (f.clone(), false)
        }
        else {
            let counter = self.filename_to_id.len() as u32;
            self.filename_to_id.insert(p, FileId(counter));
            (FileId(counter), true)
        }
    }
}