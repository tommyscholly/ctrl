use cranelift::codegen::entity::EntityRef;
use cranelift::codegen::ir::types::*;
use cranelift::codegen::ir::{AbiParam, Function, InstBuilder, UserFuncName};
// use cranelift::codegen::isa::CallConv;
use cranelift::codegen::settings;
use cranelift::codegen::verifier::verify_function;
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{default_libcall_names, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

pub fn generate() -> Result<(), Box<dyn std::error::Error>> {
    let flags = settings::Flags::new(settings::builder());
    let isa_builder = cranelift_native::builder().expect("arch isnt supported");
    let isa = isa_builder.finish(flags).expect("isa builder not finished");

    let object_builder = ObjectBuilder::new(isa, "test", default_libcall_names())
        .expect("object builder not supported");

    let mut module = ObjectModule::new(object_builder);

    let mut sig = module.make_signature();
    sig.returns.push(AbiParam::new(I32));

    let func_id = module.declare_function("tester", Linkage::Export, &sig)?;

    {
        let mut ctx = module.make_context();
        ctx.func.signature = sig.clone();

        let mut fb_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fb_ctx);

        let block0 = builder.create_block();
        let block1 = builder.create_block();

        let x = Variable::new(0);
        let z = Variable::new(1);

        builder.declare_var(x, I32);
        builder.declare_var(z, I32);
        builder.append_block_params_for_function_params(block0);

        builder.switch_to_block(block0);
        builder.seal_block(block0);

        builder.switch_to_block(block1);

        let arg1 = builder.use_var(x);
        let arg2 = builder.ins().iconst(I32, 5);
        let ret = builder.ins().iadd(arg1, arg2);
        builder.def_var(z, ret);

        let ret_arg = builder.use_var(z);
        builder.ins().return_(&[ret_arg]);
        builder.seal_block(block1);

        builder.finalize();

        module.define_function(func_id, &mut ctx)?;
    }

    let object = module.finish();
    std::fs::write("example.o", object.emit()?)?;
    println!("Object file 'example.o' generated successfully.");

    // let flags = settings::Flags::new(settings::builder());
    // let res = verify_function(&func, &flags);
    // println!("{}", func.display());
    // if let Err(errors) = res {
    //     panic!("{}", errors);
    // }

    Ok(())
}
