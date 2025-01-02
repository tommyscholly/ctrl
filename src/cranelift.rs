use std::collections::HashMap;

use cranelift::codegen::entity::EntityRef;
use cranelift::codegen::ir::types::{Type, F64, I32, I64, I8};
use cranelift::codegen::ir::{AbiParam, InstBuilder};
use cranelift::codegen::settings;
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift::prelude::{IntCC, Value};
use cranelift_module::{default_libcall_names, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

use anyhow::Result;

use crate::parse::{
    Block as BlockExpr, Bop, BuiltinType, Expression, Function as Func, Literal, Type as _, T,
};

pub struct Ctx<'a> {
    variables: HashMap<String, Variable>,
    function_ids: &'a HashMap<String, FuncId>,
    variable_counter: usize,
}

impl<'a> Ctx<'a> {
    fn new(function_ids: &'a HashMap<String, FuncId>) -> Self {
        Self {
            variables: HashMap::new(),
            function_ids,
            variable_counter: 0,
        }
    }

    fn declare_variable(
        &mut self,
        name: &str,
        builder: &mut FunctionBuilder,
        ty: Type,
    ) -> Variable {
        let var = Variable::new(self.variable_counter);
        self.variable_counter += 1;
        builder.declare_var(var, ty);
        self.variables.insert(name.to_string(), var);
        var
    }

    fn get_variable(&self, name: &str) -> Option<Variable> {
        self.variables.get(name).copied()
    }
}

// converts a T to a cranelift Type
// option represents the unit type
fn type_to_cranelift(ty: &T) -> Option<Type> {
    use T::{BuiltIn, Function, Hole, Record, Unit};

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

struct Translator<'a> {
    builder: FunctionBuilder<'a>,
    module: &'a mut ObjectModule,
    ctx: Ctx<'a>,
}

impl Translator<'_> {
    fn translate_literal(&mut self, literal: &Literal) -> Value {
        match literal {
            Literal::Bool(b) => self.builder.ins().iconst(I8, i64::from(*b)),
            Literal::Int(i) => self.builder.ins().iconst(I32, i64::from(*i)),
        }
    }

    fn translate_assignment(&mut self, ident: &str, binding: &Expression, ty: &T) {
        let val = self.translate_expression(binding);
        let var =
            self.ctx
                .declare_variable(ident, &mut self.builder, type_to_cranelift(ty).unwrap());
        self.builder.def_var(var, val);
    }

    fn translate_infix(&mut self, operation: &Bop, lhs: &Expression, rhs: &Expression) -> Value {
        let left_val = self.translate_expression(lhs);
        let right_val = self.translate_expression(rhs);

        match operation {
            Bop::Plus => self.builder.ins().iadd(left_val, right_val),
            Bop::Min => self.builder.ins().isub(left_val, right_val),
            Bop::Mul => self.builder.ins().imul(left_val, right_val),
            Bop::Div => self.builder.ins().sdiv(left_val, right_val),
            Bop::Eql => self.builder.ins().icmp(IntCC::Equal, left_val, right_val),
            Bop::Neq => self
                .builder
                .ins()
                .icmp(IntCC::NotEqual, left_val, right_val),
            Bop::Lt => self
                .builder
                .ins()
                .icmp(IntCC::SignedLessThan, left_val, right_val),
            Bop::Le => self
                .builder
                .ins()
                .icmp(IntCC::SignedLessThanOrEqual, left_val, right_val),
            Bop::Gt => self
                .builder
                .ins()
                .icmp(IntCC::SignedGreaterThan, left_val, right_val),
            Bop::Ge => {
                self.builder
                    .ins()
                    .icmp(IntCC::SignedGreaterThanOrEqual, left_val, right_val)
            }
        }
    }

    fn translate_if_else(
        &mut self,
        cond: &Expression,
        then_block_expr: &BlockExpr,
        else_block_opt: &Option<BlockExpr>,
    ) {
        let cond_val = self.translate_expression(cond);

        let then_block = self.builder.create_block();
        let bottom_block = self.builder.create_block();

        if let Some(else_block_expr) = else_block_opt {
            let else_block = self.builder.create_block();
            self.builder
                .ins()
                .brif(cond_val, then_block, &[], else_block, &[]);

            self.builder.switch_to_block(else_block);
            self.builder.seal_block(else_block);

            if !self.translate_block(else_block_expr) {
                self.builder.ins().jump(bottom_block, &[]);
            }
        } else {
            self.builder
                .ins()
                .brif(cond_val, then_block, &[], bottom_block, &[]);
        };

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);

        if !self.translate_block(then_block_expr) {
            self.builder.ins().jump(bottom_block, &[]);
        }

        self.builder.switch_to_block(bottom_block);
        self.builder.seal_block(bottom_block);
    }

    // bool corresponds to if one of the instructions in the block was a return
    fn translate_block(&mut self, b: &BlockExpr) -> bool {
        let mut had_return = false;
        for inst in &b.instructions {
            if let Expression::Return(_) = inst {
                had_return = true;
            }

            let _ = self.translate_expression(inst);
        }

        had_return
    }

    fn translate_call(&mut self, name: &str, args: &[Expression]) -> Value {
        let arg_vals: Vec<Value> = args
            .iter()
            .map(|expr| self.translate_expression(expr))
            .collect();

        let func_id = self
            .ctx
            .function_ids
            .get(name)
            .expect("tried to call a function that does not exist");

        let func_ref = self
            .module
            .declare_func_in_func(*func_id, self.builder.func);

        let call = self.builder.ins().call(func_ref, &arg_vals);
        self.builder.inst_results(call)[0]
    }

    fn translate_expression(&mut self, expr: &Expression) -> Value {
        match expr {
            Expression::Literal(literal) => self.translate_literal(literal),
            Expression::Assignment { ident, binding } => {
                let ty = expr.type_of(&HashMap::new());
                self.translate_assignment(ident, binding, &ty);
                self.builder.ins().iconst(I64, 0) // placeholder nullptr
            }
            Expression::Identifier(name) => {
                if let Some(var) = self.ctx.get_variable(name) {
                    self.builder.use_var(var)
                } else {
                    panic!("undefined identifier {name}");
                }
            }
            Expression::Infix {
                operation,
                lhs,
                rhs,
            } => self.translate_infix(operation, lhs, rhs),
            Expression::Return(expr) => {
                let return_val = self.translate_expression(expr);
                self.builder.ins().return_(&[return_val]);
                return_val
            }
            Expression::Block(b) => {
                self.translate_block(b);
                self.builder.ins().iconst(I64, 0) // placeholder nullptr
            }
            Expression::Call(function_name, args) => self.translate_call(function_name, args),
            Expression::IfElse {
                cond,
                then_block,
                else_block,
            } => {
                self.translate_if_else(cond, then_block, else_block);
                self.builder.ins().iconst(I64, 0) // placeholder nullptr
            }
            _ => unimplemented!(),
        }
    }
}

pub struct Compiler<'a> {
    module_name: &'a str,
    module: ObjectModule,
    function_ids: HashMap<String, FuncId>,
}

impl<'a> Compiler<'a> {
    pub fn new(module_name: &'a str) -> Self {
        let flags = settings::Flags::new(settings::builder());
        let isa_builder = cranelift_native::builder().expect("arch isnt supported");
        let isa = isa_builder.finish(flags).expect("isa builder not finished");

        let object_builder = ObjectBuilder::new(isa, "test", default_libcall_names())
            .expect("object builder not supported");

        let module = ObjectModule::new(object_builder);

        Self {
            module_name,
            module,
            function_ids: HashMap::new(),
        }
    }

    fn translate_function(&mut self, func: &Func) -> Result<()> {
        let param_tys: Vec<Type> = func
            .params
            .iter()
            .filter_map(|(_, ty)| type_to_cranelift(ty))
            .collect();

        let mut func_sig = self.module.make_signature();
        for ty in &param_tys {
            func_sig.params.push(AbiParam::new(*ty));
        }

        if let Some(ty) = type_to_cranelift(&func.return_ty) {
            func_sig.returns.push(AbiParam::new(ty));
        }

        let func_id = self
            .module
            .declare_function(&func.name, Linkage::Export, &func_sig)?;

        // individual context for the function
        let mut func_ctx = self.module.make_context();
        func_ctx.func.signature = func_sig.clone();

        // create the function builder context
        let mut fb_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut func_ctx.func, &mut fb_ctx);

        let block = builder.create_block();
        builder.append_block_params_for_function_params(block);
        builder.switch_to_block(block);
        builder.seal_block(block);

        // insert the function id into our compiler struct function id map
        // this way, functions calling other functions (a common occurance in programming languages)
        // can find their signatures
        self.function_ids.insert(func.name.clone(), func_id);
        // translation level context to track variables inside the function
        let mut ctx = Ctx::new(&self.function_ids);

        for (idx, (name, _)) in func.params.iter().enumerate() {
            let param_var = ctx.declare_variable(name, &mut builder, param_tys[idx]);
            let param_val = builder.block_params(block)[idx];
            builder.def_var(param_var, param_val);
        }

        let mut trans = Translator {
            builder,
            module: &mut self.module,
            ctx,
        };

        for expr in &func.body.instructions {
            let _ = trans.translate_expression(expr);
        }

        trans.builder.finalize();
        self.module.define_function(func_id, &mut func_ctx)?;

        Ok(())
    }

    // compilers are single use, this consumes itself after translating
    pub fn translate(mut self, ast: Vec<Expression>) -> Result<()> {
        for expr in ast {
            match expr {
                Expression::Function(func) => self.translate_function(&func)?,
                t => panic!("top level must be function, got {t:?}"),
            }
        }

        let object = self.module.finish();
        std::fs::write(format!("{}.o", self.module_name), object.emit()?)?;
        Ok(())
    }
}
