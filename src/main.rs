use std::{path::Path, env::args, fs};

use ast::{ProgramFile, Expr, Constant};
use inkwell::{context::Context, AddressSpace, values::BasicMetadataValueEnum, targets::{Target, InitializationConfig, RelocMode, CodeModel, TargetTriple, FileType}, OptimizationLevel};
use lalrpop_util::lalrpop_mod;

use crate::{ast_visualisator::get_program_tree, symbols::RawSymbols};
pub mod ast;
pub mod types;
pub mod symbols;
pub mod codegen;
pub mod ast_visualisator;
pub mod semtree;
pub mod errors;

lalrpop_mod!(pub grammar);
fn main() {
    if args().count() < 2 {panic!("Provide at least 1 file in command line!")}
    let file = args().nth(1).unwrap();
    let text = fs::read_to_string(&file).unwrap();
    let t = grammar::ProgramBlockParser::new().parse(&text).unwrap();
    println!("{:?}", t);
    println!("{}", get_program_tree(&t));
    let q = RawSymbols::new(&Path::new(&file).file_name().unwrap().to_string_lossy(), &t);
    println!("{:?}", q);
    
    try_compile_program(t, &file);
}

pub fn try_compile_program(input: ProgramFile, output_filename: &str) {
    let context = Context::create();
    let module = context.create_module(output_filename);
    let builder = context.create_builder();

    let puts_type = context.i32_type().fn_type(&[inkwell::types::BasicMetadataTypeEnum::PointerType( context.i8_type().ptr_type(AddressSpace::default()))], false);
    let puts = module.add_function("puts", puts_type, Some(inkwell::module::Linkage::External));

    for decls in input.declarations {
        match decls {
            ast::Declaration::Function(func) => {
                let fn_type = context.void_type().fn_type(&[], false);
                let llvm_fn = module.add_function(&func.function_name, fn_type, None);
                let t = context.append_basic_block(llvm_fn, "");
                builder.position_at_end(t);
                for stmts in func.body {
                    match stmts {
                        ast::Statement::CodeBlock(_, _ ) => todo!(),
                        ast::Statement::Print(_, expr) => {
                            if let Expr::Constant(_, Constant::String(string)) = *expr {
                                let p = builder.build_global_string_ptr(&string, "");
                                builder.build_call(puts, &[BasicMetadataValueEnum::PointerValue(p.as_pointer_value())], "puts_call");
                                
                            }
                        },
                        ast::Statement::Assignment(_, _, _) => todo!(),
                        ast::Statement::If(_, _, _, _) => todo!(),
                        ast::Statement::While(_, _, _) => todo!(),
                        ast::Statement::RepeatUntil(_, _, _) => todo!(),
                        ast::Statement::VarDecl(_, _) => todo!(),
                        ast::Statement::Empty() => todo!(),
                        ast::Statement::FunctionCall(_, _) => todo!(),
                    }
                }
                builder.build_return(None);
            },
            ast::Declaration::ExternFunction(_) => todo!(),
            ast::Declaration::TypeDeclSection(_) => todo!(),
        }
    }
    let res = module.verify();
    println!("{}", module.print_to_string().to_str().unwrap());
    match res {
        Ok(_) => println!("Code is valid!"),
        Err(x) => {println!("Code not valid! {}", x.to_str().unwrap()); panic!()},
    }
    Target::initialize_x86(&InitializationConfig::default());
    let opt = OptimizationLevel::Default;
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;
    let target = Target::from_name("x86-64").unwrap();
    let target_machine = target.create_target_machine(
    &TargetTriple::create("x86_64-pc-linux-gnu"),
    "x86-64",
    "+avx2",
    opt,
    reloc,
    model).unwrap();
    let path = String::from(output_filename) + ".o";
    let path = Path::new(&path);
    let path_asm = String::from(output_filename) + ".asm";
    let path_asm = Path::new(&path_asm);
    target_machine.write_to_file(&module, FileType::Object, &path).unwrap();
    println!("Emit object file to {}", path.to_string_lossy());
    target_machine.write_to_file(&module, FileType::Assembly, &path_asm).unwrap();
    println!("Emit asm file to {}", path_asm.to_string_lossy());

    
}


#[cfg(test)]
mod tests {
    use crate::{grammar, ast::{Declaration, Statement}};

    #[test]
    fn correct_if_parsing() {
        let program = r##"
        begin
        if 123 then if 321 then print("trl") else print("ltr"); 
        end.
        "##;
        let res =  grammar::ProgramBlockParser::new().parse(program).unwrap();
        let m = res.declarations.iter().find(|&x| {if let Declaration::Function(f) = x.clone() {
            f.function_name == "main"
        } else {false}}).unwrap();
        if let Declaration::Function(f) = m {
            let external_if = f.body.first().unwrap();
            if let Statement::If(_, _, d, c) = external_if {
                assert!(c.is_none());
                if let Statement::If(_, _, _d, c) = &(**d) {
                    assert!(c.is_some())
                }
            }
            else {panic!()}
        } else {panic!()}
    }
}