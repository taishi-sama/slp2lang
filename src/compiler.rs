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

use crate::{ast::{self, ProgramFile}, ast_visualisator, semtree::SemanticTree, semtree_visualisator, symbols::{ContextSymbolResolver, Id, Symbols, TypeResolverGenerator}};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct FileId(pub u32);
#[derive(Debug, Clone)]
pub struct Compiler{
    includes: Vec<PathBuf>,
    pub filename_to_id: Arc<HashMap<String, FileId>>,
    id_to_filepath: HashMap<FileId, PathBuf>,
    asts: HashMap<FileId, Arc<ProgramFile>>,
    pub deps: Arc<HashMap<FileId, Vec<FileId>>>,
    queue_on_check: VecDeque<FileId>
}

impl Compiler {
    pub fn new(includes: Vec<PathBuf>) -> Self {
        Self{filename_to_id: Default::default(), asts: Default::default(), deps: Default::default(), queue_on_check: Default::default(), includes, id_to_filepath: Default::default() }
    }
    pub fn start_compilation(&mut self, initial_file: PathBuf) -> anyhow::Result<()> {
        let text = fs::read_to_string(&initial_file)?;
        let ast = crate::grammar::ProgramBlockParser::new().parse(&text).unwrap();
        
        let fid = self.path_to_fileid_mut(&initial_file);
        let pf = Arc::new(ast);
        println!("{}", ast_visualisator::get_program_tree(&pf));
        self.asts.insert(fid.0, pf.clone());
        self.check_childs(&fid.0, &pf )?;
        self.check_queue()?;
        Ok(())
    }
    pub fn continue_compilation(&mut self) -> anyhow::Result<Vec<SemanticTree>> {
        let mut t = TypeResolverGenerator::new(&self);
        for (ids, deps) in self.deps.iter() {
            t.fill(&self.asts[ids], ids.clone())
        }
        let type_resolver = t.resolve();
        let type_resolver_arc = Arc::new(type_resolver);
        let mut syms: HashMap<FileId, Symbols> = HashMap::new();
        
        for (ids, deps) in self.deps.iter() {
            let p = Self::path_into_string(&self.id_to_filepath[ids]);
            let rs = Symbols::new(&p, &self.asts[ids], ids.clone(), &type_resolver_arc)?;
            //let ars = Arc::new(rs);
            syms.insert(*ids, rs);
        }
        let syms: HashMap<_, _> = syms.into_iter().map(|(k, v)|(k, Arc::new(v))).collect();
        
        let mut semtrees = vec![];
        for (ids, deps) in self.deps.iter() {
            let p = Self::path_into_string(&self.id_to_filepath[ids]);

            println!("Compiling {}", self.id_to_filepath[ids].to_string_lossy());
            let ctx = ContextSymbolResolver::new(syms[ids].clone(), 
                deps.iter().map(|x|syms[x].clone()).collect());
            let semtree = SemanticTree::new(&self.asts[ids], ctx, Id(p), ids.clone(), type_resolver_arc.clone());
            let semtree_unwrap = semtree.unwrap();
            
            //if semtree_unwrap.symbols.main_file_symbols.func_decls.contains_key(&Id("main".to_string())) {
            //    println!("{}", semtree_visualisator::get_program_root(&semtree_unwrap.root));
            //}

            semtrees.push(semtree_unwrap);
            
        }
        Ok(semtrees)
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
    fn check_childs(&mut self, source_file: &FileId, f: &ProgramFile) -> anyhow::Result<()> {
        println!("Checking {}", self.id_to_filepath[&source_file].to_string_lossy());
        let mut deps = vec![];
        for u in &f.uses {
            match u {
                ast::Usings::Name(_, n) => {
                    println!("Checking dep: {n}");
                    let f = self.find_first_applicable_child(n).unwrap();
                    let (fid, cond) = self.path_to_fileid_mut(&f);
                    deps.push(fid);
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
        Arc::get_mut(&mut self.deps).unwrap().insert(*source_file, deps);
        Ok(())
    }
    fn check_queue(&mut self) -> anyhow::Result<()> {
        while let Some(entry) = self.queue_on_check.pop_front() {
            let t = self.asts[&entry].clone();
            self.check_childs(&entry ,&t)?;
        }
        Ok(())
    }
    fn path_into_string(path: &Path) -> String {
        path.file_stem().unwrap().to_string_lossy().into_owned()
    } 
    fn path_to_fileid_mut(&mut self, path: &Path) -> (FileId, bool) {
        //Properly do hierarchy
        let p = Self::path_into_string(path);
        if let Some(f) = self.filename_to_id.get(&p) {
            (f.clone(), false)
        }
        else {
            
            let counter = self.filename_to_id.len() as u32;
            Arc::get_mut(&mut self.filename_to_id).unwrap().insert(p, FileId(counter));
            self.id_to_filepath.insert(FileId(counter), path.to_path_buf());
            (FileId(counter), true)
        }
    }
}