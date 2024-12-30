use cranelift::codegen::entity::EntityRef;
use cranelift::codegen::ir::types::*;
use cranelift::codegen::ir::{AbiParam, Block, Function, InstBuilder, UserFuncName};
use cranelift::codegen::settings;
use cranelift::codegen::verifier::verify_function;
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift::prelude::Imm64;
use cranelift_module::{default_libcall_names, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

use crate::parse::{Block as BlockExpr, BuiltinType, Expression, Function as Func, Literal, T};
use anyhow::Result;

pub struct Ctx {
    var_idx: usize,
}

impl Ctx {
    fn idx(&mut self) -> usize {
        let index = self.var_idx;
        self.var_idx += 1;
        index
    }
}

// converts a T to a cranelift Type
// option represents the unit type
fn type_to_cranelift(ty: &T) -> Option<Type> {
    use T::*;

    match ty {
        Hole => panic!("Hit bottom type when translating to IR"),
        Unit => None,
        BuiltIn(b) => match b {
            BuiltinType::Int => Some(I32),
            BuiltinType::Float => Some(F64),
            BuiltinType::String | BuiltinType::Array => Some(I64), // ptr
            BuiltinType::Char | BuiltinType::Bool => Some(I8),
        },
        Record(_)
        | Function {
            param_tys: _,
            return_ty: _,
        } => Some(I64), // ptr
    }
}

fn translate_function(func: &Func, module: &mut ObjectModule) -> Result<()> {
    let param_tys: Vec<Type> = func
        .params
        .iter()
        .filter_map(|(_, ty)| type_to_cranelift(ty))
        .collect();

    let mut func_sig = module.make_signature();
    for ty in param_tys {
        func_sig.params.push(AbiParam::new(ty))
    }

    if let Some(ty) = type_to_cranelift(&func.return_ty) {
        func_sig.returns.push(AbiParam::new(ty));
    }

    let func_id = module.declare_function(&func.name, Linkage::Export, &func_sig)?;
    let mut func_ctx = module.make_context();
    func_ctx.func.signature = func_sig.clone();

    let mut fb_ctx = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut func_ctx.func, &mut fb_ctx);

    Ok(())
}

fn translate_block(
    b: BlockExpr,
    module: &mut ObjectModule,
    builder: &mut FunctionBuilder<'_>,
    ctx: &mut Ctx,
) -> Result<Block> {
    let object_block = builder.create_block();
    for inst in b.instructions {
        translate(inst, module, builder, ctx)?;
    }

    Ok(object_block)
}

pub fn translate(
    ast: Expression,
    module: &mut ObjectModule,
    builder: &mut FunctionBuilder<'_>,
    ctx: &mut Ctx,
) -> Result<()> {
    match ast {
        Expression::Function(func) => translate_function(&func, module)?,
        Expression::Literal(l) => match l {
            Literal::Int(i) => {
                let var = Variable::new(ctx.idx());
                builder.declare_var(var, I32);

                let num: i64 = i.into();
                let val = builder.ins().iconst(I32, num);
                builder.def_var(var, val);
            }
            Literal::Bool(b) => {
                let var = Variable::new(ctx.idx());
                builder.declare_var(var, I8);

                let bool_ = if b { 1 } else { 0 };
                let val = builder.ins().iconst(I8, bool_);
                builder.def_var(var, val);
            }
        },
        Expression::Identifier(i) => todo!(),
        Expression::Assignment { ident, binding } => todo!(),
        Expression::Return(e) => todo!(),
        Expression::IfElse {
            cond,
            then_block,
            else_block,
        } => todo!(),
        Expression::Infix {
            operation,
            lhs,
            rhs,
        } => todo!(),
        Expression::Block(b) => todo!(),
    }

    Ok(())
}

pub fn ast_to_ir(ast: Vec<Expression>) -> Result<()> {
    let flags = settings::Flags::new(settings::builder());
    let isa_builder = cranelift_native::builder().expect("arch isnt supported");
    let isa = isa_builder.finish(flags).expect("isa builder not finished");

    let object_builder = ObjectBuilder::new(isa, "test", default_libcall_names())
        .expect("object builder not supported");

    let mut module = ObjectModule::new(object_builder);

    todo!()
}

pub fn generate() -> Result<()> {
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
