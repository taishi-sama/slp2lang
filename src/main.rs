use std::{path::Path, env::args, fs};

use ast::{Program, Expr, Constant};
use inkwell::{context::Context, AddressSpace, values::BasicMetadataValueEnum, targets::{Target, InitializationConfig, RelocMode, CodeModel, TargetTriple, FileType}, OptimizationLevel};
use lalrpop_util::lalrpop_mod;
pub mod ast;
pub mod typechecker;
pub mod typed_ast;
pub mod codegen;

lalrpop_mod!(pub grammar);
fn main() {
    if args().count() < 2 {panic!("Provide at least 1 file in command line!")}
    let file = args().nth(1).unwrap();
    let text = fs::read_to_string(&file).unwrap();
    let t = grammar::ProgramBlockParser::new().parse(&text).unwrap();
    println!("{:?}", t);
    try_compile_program(t, &file);
}

pub fn try_compile_program(input: Program, output_filename: &str) {
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
                    }
                }
                builder.build_return(None);
            },
            ast::Declaration::ExternFunction(_) => todo!(),
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