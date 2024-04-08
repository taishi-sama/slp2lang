use std::{env::args, fs::{self, create_dir}, path::Path, sync::Arc};

use codegen::{Codegen, CodegenContext};
use compiler::Compiler;
use inkwell::{
    passes::PassBuilderOptions,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
    OptimizationLevel,
};
use lalrpop_util::lalrpop_mod;

use crate::linkage::LinkerBuilder;

pub mod ast;
pub mod ast_visualisator;
pub mod codegen;
pub mod compiler;
pub mod errors;
pub mod linkage;
pub mod semtree;
pub mod semtree_visualisator;
pub mod symbols;
pub mod types;

lalrpop_mod!(pub grammar);
fn main() {
    if args().count() < 2 {
        panic!("Provide at least 1 file in command line!")
    }
    let file = args().nth(1).unwrap();

    let path = String::from(&file);
    new_compile(&file, &path);
}
pub fn new_compile(file: &str, output_filename: &str) {
    Target::initialize_x86(&InitializationConfig::default());
    let opt = OptimizationLevel::Default;
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;
    let target = Target::from_name("x86-64").unwrap();

    let target_machine = target
        .create_target_machine(
            &TargetTriple::create("x86_64-pc-linux-gnu"),
            "x86-64",
            "+avx2",
            opt,
            reloc,
            model,
        )
        .unwrap();
    let target_machine = Arc::new(target_machine);
    let cctx = CodegenContext::new();
    let mut comp = Compiler::new(vec!["./".to_owned().into(), "./std".to_owned().into()]);
    comp.start_compilation(file.into()).unwrap();
    let res = comp.continue_compilation().unwrap();
    let mut modules = vec![];
    for i in &res {
        let cdgn: Codegen = Codegen::new(&cctx, &i.semtree_name.0, target_machine.clone());
        cdgn.compile_semtree(i);
        match cdgn.module.verify() {
            Ok(_) => {
                println!(
                    "Module {}:--------------------------------------- \n{}",
                    i.semtree_name.0,
                    cdgn.module.print_to_string().to_string_lossy()
                )
            }
            Err(e) => {
                println!(
                    "Error in module {}: {}",
                    i.semtree_name.0,
                    e.to_string_lossy()
                );
                println!(
                    "Module {}:--------------------------------------- \n{}",
                    i.semtree_name.0,
                    cdgn.module.print_to_string().to_string_lossy()
                );

                break;
            }
        }
        modules.push(cdgn);
    }
    modules.reverse();
    let main_module = modules.pop().unwrap();
    modules.reverse();
    for m in modules {
        main_module.module.link_in_module(m.module).unwrap();
    }
    target_machine.get_target_data();
    main_module
        .module
        .run_passes("mem2reg", &target_machine, PassBuilderOptions::create())
        .unwrap();
    println!("{}", main_module.module.print_to_string().to_string_lossy());
    let p = Path::new(output_filename);

    let output_dir = p
    .parent()
    .unwrap()
    .join("target");
    if !Path::exists(&output_dir) {
        create_dir(&output_dir).unwrap();
    }
    
    let filename = p.file_stem().unwrap();
    let object_output = (&output_dir).join(Path::new(filename).with_extension("o"));
    let executable_output = (&output_dir).join(filename);

    target_machine
        .write_to_file(&main_module.module, FileType::Object, &object_output)
        .unwrap();
    println!("Emit object file to {}", object_output.to_string_lossy());
    let linker =
        LinkerBuilder::new_linux_x86_64().link_gnu_linker_flavor(&object_output, &executable_output);
    linker.unwrap();
    println!(
        "Linkage complete... File available at {:}",
        executable_output.to_string_lossy()
    )
    //target_machine
    //    .write_to_file(&main_module.module, FileType::Assembly, &path_asm)
    //    .unwrap();
    //println!("Emit asm file to {}", path_asm.to_string_lossy());
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Declaration, Statement},
        grammar,
    };

    #[test]
    fn correct_if_parsing() {
        let program = r##"
        begin
        if 123 then if 321 then print("trl") else print("ltr"); 
        end.
        "##;
        let res = grammar::ProgramBlockParser::new().parse(program).unwrap();
        let m = res
            .declarations
            .iter()
            .find(|&x| {
                if let Declaration::Function(f) = x.clone() {
                    f.function_name == "main"
                } else {
                    false
                }
            })
            .unwrap();
        if let Declaration::Function(f) = m {
            let external_if = f.body.first().unwrap();
            if let Statement::If(_, _, d, c) = external_if {
                assert!(c.is_none());
                if let Statement::If(_, _, _d, c) = &(**d) {
                    assert!(c.is_some())
                }
            } else {
                panic!()
            }
        } else {
            panic!()
        }
    }
}
