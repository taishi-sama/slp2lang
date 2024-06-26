// Do syntax analysys of initial file
// Take usages
// Do syntax analysys of usages recursive
// Build symbol table of usages
// Combine tables
// Compile all depedencies using cross-combined symbol tables
// Link them using LLVM internal linker
// Link resulting .obj file into executable

use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    fs,
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
};

use anyhow::Ok;

use crate::{
    ast::{self, ProgramFile},
    ast_visualisator,
    buildins::BuildInModule,
    error_handler::ErrorHandler,
    semtree::SemanticTree,
    symbols::{Id, TypeResolverGenerator},
};
const COMPILER_BUILDINS_MODULE_FID: FileId = FileId(0);
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct FileId(pub u32);
#[derive(Debug, Clone)]
pub struct Compiler {
    error_handler: ErrorHandler,
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
            error_handler: ErrorHandler::new(),
        }
    }
    pub fn start_compilation(&mut self, initial_file: PathBuf) -> anyhow::Result<()> {
        let text = fs::read_to_string(&initial_file)?;
        self.add_fid_as("$build-in$", COMPILER_BUILDINS_MODULE_FID);
        let fid = self.path_to_fileid_mut(&initial_file);

        let ast: Result<
            ProgramFile,
            lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token, &str>,
        > = crate::grammar::ProgramBlockParser::new().parse(fid.0.clone(), &text);
        if let Err(e) = ast {
            self.error_handler.add_syntax_error(fid.0.clone(), &e, &self.id_to_filepath);
            self.error_handler.display_errors()?;
            panic!()
        }
        let ast = ast.unwrap();
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
        let type_resolver_raw = t.resolve();
        if type_resolver_raw.is_err() {
            for e in type_resolver_raw.err().unwrap() {
                self.error_handler.add_error(e, &self.id_to_filepath);
            }
            self.error_handler.display_errors()?;
            panic!()
        }
        let mut type_resolver = type_resolver_raw.unwrap();
        for (ids, _deps) in self.deps.iter() {
            println!(
                "Resolving {}",
                type_resolver.reverse_filename_translation[ids]
            );
            type_resolver.fill_function_decl(&self.asts[ids], ids)?
        }
        println!("Function declarations resolved");
        let type_resolver_arc = Arc::new(type_resolver);
        let buildins = Arc::new(RefCell::new(BuildInModule::new(type_resolver_arc.clone())));
        let mut semtrees = vec![];
        for (ids, _deps) in self.deps.iter() {
            let p = Self::path_into_string(&self.id_to_filepath[ids]);

            println!("Compiling {}", self.id_to_filepath[ids].to_string_lossy());

            let semtree = SemanticTree::new(
                &self.asts[ids],
                Id(p),
                ids.clone(),
                type_resolver_arc.clone(),
                buildins.clone(),
            );
            let semtree_unwrap = match semtree {
                std::result::Result::Ok(st) => st,
                Err(err) => {
                    for e in err {
                        self.error_handler.add_error(e, &self.id_to_filepath);
                    }
                    self.error_handler.display_errors()?;
                    panic!()
                }
            };
            semtrees.push(semtree_unwrap);
        }
        Ok(semtrees)
    }
    fn find_first_applicable_child(&self, filename: &str) -> Option<PathBuf> {
        for i in &self.includes {
            let t = Path::join(&i, format!("{}.slp2", filename));
            //println!("Checking {}...", t.to_string_lossy());
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
                            .parse(fid.clone(), &text);
                        if let Err(e) = ast {
                            self.error_handler.add_syntax_error(fid.clone(), &e, &self.id_to_filepath);
                            self.error_handler.display_errors()?;
                            panic!()
                        }
                        let ast = ast.unwrap();
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
        //let counter = self.filename_to_id.len() as u32 + 1;
        Arc::get_mut(&mut self.filename_to_id)
            .unwrap()
            .insert(str.to_string(), fid);
        Arc::get_mut(&mut self.id_to_filename)
            .unwrap()
            .insert(fid, str.to_string());
        self.id_to_filepath
            .insert(fid, PathBuf::from_str(str).unwrap());
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
