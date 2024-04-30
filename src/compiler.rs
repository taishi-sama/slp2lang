// Do syntax analysys of initial file
// Take usages
// Do syntax analysys of usages recursive
// Build symbol table of usages
// Combine tables
// Compile all depedencies using cross-combined symbol tables
// Link them using LLVM internal linker
// Link resulting .obj file into executable

use std::{
    cell::RefCell, collections::{HashMap, VecDeque}, fs::{self, read_to_string, File}, io::Read, path::{Path, PathBuf}, str::FromStr, sync::Arc
};

use anyhow::Ok;
use source_span::{fmt::Formatter, DefaultMetrics, SourceBuffer};

use crate::{
    ast::{self, ProgramFile}, ast_visualisator, buildins::BuildInModule, error_handler::InfileLoc, semtree::SemanticTree, semtree_visualisator, symbols::{Id, TypeResolverGenerator}
};
const COMPILER_BUILDINS_MODULE_FID: FileId = FileId(0);
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct FileId(pub u32);
#[derive(Debug, Clone)]
pub struct Compiler {
    includes: Vec<PathBuf>,
    pub filename_to_id: Arc<HashMap<String, FileId>>,
    pub id_to_filename: Arc<HashMap<FileId, String>>,
    id_to_filepath: HashMap<FileId, PathBuf>,
    asts: HashMap<FileId, Arc<ProgramFile>>,
    pub deps: Arc<HashMap<FileId, Vec<FileId>>>,
    queue_on_check: VecDeque<FileId>,
}

impl Compiler {
    pub fn new(includes: Vec<PathBuf>) -> Self {
        Self {
            filename_to_id: Default::default(),
            asts: Default::default(),
            deps: Default::default(),
            queue_on_check: Default::default(),
            includes,
            id_to_filepath: Default::default(),
            id_to_filename: Default::default(),
        }
    }
    pub fn start_compilation(&mut self, initial_file: PathBuf) -> anyhow::Result<()> {
        let text = fs::read_to_string(&initial_file)?;
        self.add_fid_as("$build-in$", COMPILER_BUILDINS_MODULE_FID);
        let fid = self.path_to_fileid_mut(&initial_file);

        let ast = crate::grammar::ProgramBlockParser::new()
            .parse(fid.0.clone(), &text)
            .unwrap();
        let pf = Arc::new(ast);
        println!("{}", ast_visualisator::get_program_tree(&pf));
        self.asts.insert(fid.0, pf.clone());
        self.check_childs(&fid.0, &pf)?;
        self.check_queue()?;
        Ok(())
    }
    pub fn continue_compilation(&mut self) -> anyhow::Result<Vec<SemanticTree>> {
        let mut t = TypeResolverGenerator::new(&self);
        for (ids, _deps) in self.deps.iter() {
            t.fill(&self.asts[ids], ids.clone())
        }
        let mut type_resolver = t.resolve();


        for (ids, _deps) in self.deps.iter() {
            println!("Resolving {}", type_resolver.reverse_filename_translation[ids]);
            type_resolver.fill_function_decl(&self.asts[ids], ids)?
        }
        println!("Function declarations resolved");
        let type_resolver_arc = Arc::new(type_resolver);
        let buildins = Arc::new(RefCell::new(BuildInModule::new(type_resolver_arc.clone())));
        let mut semtrees = vec![];
        for (ids, deps) in self.deps.iter() {
            let p = Self::path_into_string(&self.id_to_filepath[ids]);

            println!("Compiling {}", self.id_to_filepath[ids].to_string_lossy());

            let semtree = SemanticTree::new(
                &self.asts[ids],
                Id(p),
                ids.clone(),
                type_resolver_arc.clone(),
                buildins.clone()
            );
            let semtree_unwrap = match semtree {
                std::result::Result::Ok(st) => st,
                Err(err) => {
                    for e in err {
                        match e {
                            crate::errors::SemTreeBuildErrors::BadType(l, c) => {
                                let file = read_to_string(&self.id_to_filepath[ids])?;         
                                let s = InfileLoc::from_loc(l, &file);
                                let file = File::open(&self.id_to_filepath[ids]).unwrap();
                                let chars = utf8_decode::UnsafeDecoder::new(file.bytes());
                                let metrics = source_span::DEFAULT_METRICS;
                                let buffer = SourceBuffer::new(chars, source_span::Position::default(), metrics);
                            
                                let mut form = Formatter::new();
                                buffer.iter().for_each(|_|()); 
                                form.add(s.span.clone(), Some(format!("Unknown type \"{}\"", c)), source_span::fmt::Style::Error);
                                let t = form.render(buffer.iter(), buffer.span().clone(), &metrics);
                                println!("{}", t.unwrap())
                            },
                            _ => todo!()
                        }
                    }
                    panic!("----------")
                },
            };
            

            //if semtree_unwrap
            //    .symbols
            //    .main_file_symbols
            //    .func_decls
            //    .contains_key(&Id("main".to_string()))
            //{
            //    println!(
            //        "{}",
            //        semtree_visualisator::get_program_root(&semtree_unwrap.root)
            //    );
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
        println!(
            "Checking {}",
            self.id_to_filepath[&source_file].to_string_lossy()
        );
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
                        let ast = crate::grammar::ProgramBlockParser::new()
                            .parse(fid.clone(), &text )
                            .unwrap();
                        self.asts.insert(fid, Arc::new(ast));
                        self.queue_on_check.push_back(fid);
                    } else {
                        println!("File already added: {}", f.to_string_lossy());
                    }
                }
                ast::Usings::Path(_, _) => todo!(),
            }
        }
        Arc::get_mut(&mut self.deps)
            .unwrap()
            .insert(*source_file, deps);
        Ok(())
    }
    fn check_queue(&mut self) -> anyhow::Result<()> {
        while let Some(entry) = self.queue_on_check.pop_front() {
            let t = self.asts[&entry].clone();
            self.check_childs(&entry, &t)?;
        }
        Ok(())
    }
    fn path_into_string(path: &Path) -> String {
        path.file_stem().unwrap().to_string_lossy().into_owned()
    }
    fn add_fid_as(&mut self, str: &str, fid: FileId) {
        let counter = self.filename_to_id.len() as u32 + 1;
        Arc::get_mut(&mut self.filename_to_id)
                .unwrap()
                .insert(str.to_string(), FileId(counter));
        Arc::get_mut(&mut self.id_to_filename)
                .unwrap()
                .insert(FileId(counter), str.to_string());
        self.id_to_filepath
                .insert(FileId(counter), PathBuf::from_str(str).unwrap());
        
    } 
    fn path_to_fileid_mut(&mut self, path: &Path) -> (FileId, bool) {
        //Properly do hierarchy
        let p = Self::path_into_string(path);
        if let Some(f) = self.filename_to_id.get(&p) {
            (f.clone(), false)
        } else {
            let counter = self.filename_to_id.len() as u32 + 1;
            Arc::get_mut(&mut self.filename_to_id)
                .unwrap()
                .insert(p.clone(), FileId(counter));
            Arc::get_mut(&mut self.id_to_filename)
                .unwrap()
                .insert(FileId(counter), p);
            self.id_to_filepath
                .insert(FileId(counter), path.to_path_buf());
            (FileId(counter), true)
        }
    }
}
