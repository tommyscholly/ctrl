use std::collections::HashMap;
use std::process::Command;

use anyhow::{anyhow, Result};
use convert_case::{Case, Casing};

use cranelift::codegen::entity::EntityRef;
use cranelift::codegen::ir::types::{Type, F64, I32, I64, I8};
use cranelift::codegen::ir::{AbiParam, GlobalValue, InstBuilder};
use cranelift::codegen::verify_function;
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift::prelude::{settings, Value};
use cranelift_module::{default_libcall_names, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

use crate::ir::{Body, ControlFlow, Literal, TypeInfo, TypedIR, IR, T};

mod ctrl_std;
mod types;

pub struct ScopeCtx {
    variables: HashMap<String, Variable>,
    var_counter: usize,
}

impl ScopeCtx {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            var_counter: 0,
        }
    }

    fn declare_variable(
        &mut self,
        name: &str,
        builder: &mut FunctionBuilder,
        ty: Type,
    ) -> Variable {
        let var = Variable::new(self.var_counter);
        self.var_counter += 1;
        builder.declare_var(var, ty);
        self.variables.insert(name.to_string(), var);
        var
    }

    fn get_variable(&self, name: &str) -> Option<Variable> {
        self.variables.get(name).copied()
    }
}

pub struct Ctx {
    function_ids: HashMap<String, FuncId>,
}

impl Ctx {
    pub fn new() -> Self {
        Self {
            function_ids: HashMap::new(),
        }
    }
}

pub struct FuncCodegen<'a> {
    ctx: &'a Ctx,
    scope_ctx: ScopeCtx,
    fb: FunctionBuilder<'a>,
    module: &'a mut ObjectModule,
}

impl<'a> FuncCodegen<'a> {
    pub fn new(
        ctx: &'a Ctx,
        scope_ctx: ScopeCtx,
        fb: FunctionBuilder<'a>,
        module: &'a mut ObjectModule,
    ) -> Self {
        Self {
            ctx,
            scope_ctx,
            fb,
            module,
        }
    }

    fn control_flow(&mut self, flow: ControlFlow) -> Result<()> {
        match flow {
            ControlFlow::Break => todo!(),
            ControlFlow::Continue => todo!(),
            ControlFlow::Return(ir) => {
                let return_val = self
                    .gen(ir)?
                    .expect("return value should be an expression, not a statement");
                self.fb.ins().return_(&[return_val]);
            }
        }

        Ok(())
    }

    fn literal(&mut self, literal: Literal) -> Value {
        match literal {
            Literal::Bool(b) => self.fb.ins().iconst(I8, i64::from(b)),
            Literal::Int(i) => self.fb.ins().iconst(I32, i64::from(i)),
            Literal::String(s) => todo!(),
        }
    }

    pub fn gen(&mut self, ty_ir: TypedIR) -> Result<Option<Value>> {
        let (ir, ty) = ty_ir.inner();
        match ir {
            IR::ControlFlow(flow) => self.control_flow(*flow)?,
            IR::Literal(literal) => return Ok(Some(self.literal(literal))),
            t => {
                println!("{t:?}");
                todo!();
            }
        }

        Ok(None)
    }
}

pub struct Codegen {
    name: String,
    ctx: Ctx,
    type_info: TypeInfo,
    module: ObjectModule,
}

impl Codegen {
    pub fn new(module_name: &str, type_info: TypeInfo) -> Self {
        let flags = settings::Flags::new(settings::builder());
        let isa_builder = cranelift_native::builder().expect("arch isnt supported");
        let isa = isa_builder.finish(flags).expect("isa builder not finished");

        let object_builder = ObjectBuilder::new(isa, module_name, default_libcall_names())
            .expect("object builder not supported");

        let mut module = ObjectModule::new(object_builder);
        let mut ctx = Ctx::new();
        ctrl_std::include_ctrl_stdlib(&mut module, &mut ctx.function_ids);

        Self {
            name: module_name.to_string(),
            ctx,
            module,
            type_info,
        }
    }

    fn generate_function(
        &mut self,
        name: String,
        params: Vec<(String, T)>,
        return_type: T,
        body: Body,
    ) -> Result<()> {
        let param_tys: Vec<Type> = params
            .iter()
            .filter_map(|(_, ty)| types::type_to_cranelift(ty))
            .collect();

        let mut function_signature = self.module.make_signature();
        for ty in &param_tys {
            function_signature.params.push(AbiParam::new(*ty));
        }

        if let Some(ty) = types::type_to_cranelift(&return_type) {
            function_signature.returns.push(AbiParam::new(ty));
        }

        let function_id =
            self.module
                .declare_function(&name, Linkage::Export, &function_signature)?;

        let mut function_ctx = self.module.make_context();
        function_ctx.func.signature = function_signature;

        let mut fb_ctx = FunctionBuilderContext::new();
        let mut fb = FunctionBuilder::new(&mut function_ctx.func, &mut fb_ctx);

        let block = fb.create_block();
        fb.append_block_params_for_function_params(block);
        fb.switch_to_block(block);
        fb.seal_block(block);

        self.ctx.function_ids.insert(name, function_id);

        let mut scope_ctx = ScopeCtx::new();
        for (idx, (name, _)) in params.iter().enumerate() {
            let param_var = scope_ctx.declare_variable(name, &mut fb, param_tys[idx]);
            let param_val = fb.block_params(block)[idx];
            fb.def_var(param_var, param_val);
        }

        let mut func_codegen = FuncCodegen::new(&self.ctx, scope_ctx, fb, &mut self.module);
        for ty_ir in body.inner() {
            let _ = func_codegen.gen(ty_ir);
        }

        if let Err(errors) = verify_function(func_codegen.fb.func, func_codegen.module.isa()) {
            eprintln!("Function verification failed:\n{}", errors);
            return Err(anyhow!("Function verification failed"));
        }

        println!("{}", func_codegen.fb.func.display());

        func_codegen.fb.finalize();
        self.module
            .define_function(function_id, &mut function_ctx)?;

        Ok(())
    }

    pub fn generate(mut self, ir: Vec<TypedIR>) -> Result<()> {
        for ty_ir in ir {
            let (ir, ty) = ty_ir.inner();

            match ir {
                IR::Function(name, params, body) => {
                    let T::Function {
                        param_tys,
                        return_ty,
                    } = ty
                    else {
                        unreachable!();
                    };
                    self.generate_function(name, params, *return_ty, body);
                }
                t => panic!("top level must be function or a type definition, got {t:?}"),
            };
        }

        let object = self.module.finish();
        let object_file = format!("{}.o", self.name);
        std::fs::write(&object_file, object.emit()?)?;
        let command_status = Command::new("cc")
            .args(["-fuse-ld=mold", "ctrl_std.c", &object_file, "-o", "main"])
            .status()?;

        if !command_status.success() {
            eprintln!("Linking failure");
        } else {
            std::fs::remove_file(object_file)?;
        }
        Ok(())
    }
}
