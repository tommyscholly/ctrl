use std::collections::HashMap;
use std::process::Command;

use anyhow::{anyhow, Result};

use convert_case::{Case, Casing};
use cranelift::codegen::entity::EntityRef;
use cranelift::codegen::ir::types::{Type, F64, I32, I64, I8};
use cranelift::codegen::ir::{AbiParam, GlobalValue, InstBuilder};
use cranelift::codegen::{settings, verify_function};
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift::prelude::{Block, IntCC, MemFlags, StackSlotData, StackSlotKind, Value};
use cranelift_module::{default_libcall_names, DataDescription, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

use crate::parse::{
    Array, Block as BlockExpr, Bop, BuiltinType, Expression, Function as Func, Literal, Type as _,
    T,
};
use crate::type_checker::TypeMap;

pub struct Ctx<'a> {
    variables: HashMap<String, Variable>,
    function_ids: &'a HashMap<String, FuncId>,
    strings: &'a mut HashMap<String, GlobalValue>,
    variable_counter: usize,
}

impl<'a> Ctx<'a> {
    fn new(
        function_ids: &'a HashMap<String, FuncId>,
        strings: &'a mut HashMap<String, GlobalValue>,
    ) -> Self {
        Self {
            variables: HashMap::new(),
            function_ids,
            strings,
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
    use T::{Any, Array, BuiltIn, Function, Hole, Record, TypeId, Unit};

    match ty {
        Hole => panic!("Hit bottom type when translating to IR"),
        Unit => None,
        BuiltIn(b) => match b {
            BuiltinType::Int => Some(I32),
            BuiltinType::Float => Some(F64),
            BuiltinType::String => Some(I64), // ptr
            BuiltinType::Char | BuiltinType::Bool => Some(I8),
        },
        Array(_, _) => Some(I64), // ptr
        Record(_)
        | Function {
            param_tys: _,
            return_ty: _,
        }
        | TypeId(_)
        | Any => Some(I64), // ptr
    }
}

struct Translator<'a> {
    builder: FunctionBuilder<'a>,
    ctx: Ctx<'a>,
    module: &'a mut ObjectModule,
    type_map: &'a TypeMap,
    loop_blocks: Vec<Block>, // simple stack to keep track of loop blocks
}

impl Translator<'_> {
    fn translate_literal(&mut self, literal: &Literal) -> Value {
        match literal {
            Literal::Bool(b) => self.builder.ins().iconst(I8, i64::from(*b)),
            Literal::Int(i) => self.builder.ins().iconst(I32, i64::from(*i)),
            Literal::String(s) => {
                let string_literal_id = self
                    .module
                    .declare_data(
                        &format!("string_{}", s.to_case(Case::Snake)),
                        Linkage::Local,
                        true,
                        false,
                    )
                    .expect("to declare string literal");

                let str_ptr = if let Some(global) = self.ctx.strings.get(s) {
                    *global
                } else {
                    let mut string_ctx = DataDescription::new();
                    string_ctx.define(s.as_bytes().into());

                    self.module
                        .define_data(string_literal_id, &string_ctx)
                        .expect("to define string literal");

                    let str_ptr = self
                        .module
                        .declare_data_in_func(string_literal_id, self.builder.func);

                    self.ctx.strings.insert(s.to_string(), str_ptr);

                    str_ptr
                };

                let ptr = self.builder.ins().global_value(I64, str_ptr);
                let make_ctrl_string = self.module.declare_func_in_func(
                    *self.ctx.function_ids.get("ctrl_make_string").unwrap(),
                    self.builder.func,
                );

                let len = self.builder.ins().iconst(I32, s.len() as i64);
                let call = self.builder.ins().call(make_ctrl_string, &[ptr, len]);
                let call_results = self.builder.inst_results(call);

                call_results[0]
            }
        }
    }

    fn translate_assignment(&mut self, ident: &str, binding: &Expression, ty: &T) {
        let val = self
            .translate_expression(binding)
            .expect("binding should not be a statement");
        if let Some(var) = self.ctx.get_variable(ident) {
            // handle SSA
            self.builder.def_var(var, val);
        } else {
            println!("{:?} : {:?}", binding, ty);
            let var =
                self.ctx
                    .declare_variable(ident, &mut self.builder, type_to_cranelift(ty).unwrap());
            self.builder.def_var(var, val);
        }
    }

    fn translate_infix(&mut self, operation: &Bop, lhs: &Expression, rhs: &Expression) -> Value {
        let left_val = self
            .translate_expression(lhs)
            .expect("lhs should not be a statement");
        let right_val = self
            .translate_expression(rhs)
            .expect("rhs should not be a statement");

        match operation {
            Bop::Plus => {
                if let T::BuiltIn(BuiltinType::String) = lhs.type_of(self.type_map) {
                    let concat_string = self.module.declare_func_in_func(
                        *self.ctx.function_ids.get("ctrl_concat_string").unwrap(),
                        self.builder.func,
                    );

                    let call = self
                        .builder
                        .ins()
                        .call(concat_string, &[left_val, right_val]);
                    let call_results = self.builder.inst_results(call);

                    call_results[0]
                } else {
                    self.builder.ins().iadd(left_val, right_val)
                }
            }
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
        let cond_val = self
            .translate_expression(cond)
            .expect("cond should not be a statement");

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
            } else if let Expression::Break = inst {
                if let Some(loop_bottom) = self.loop_blocks.last() {
                    self.builder.ins().jump(*loop_bottom, &[]);
                    return true;
                } else {
                    panic!("break outside of loop");
                }
            } // else if let Expression::Continue = inst {
              //     if let Some(loop_bottom) = self.loop_bottom_block {
              //         self.builder.ins().jump(loop_bottom, &[]);
              //         return false;
              //     } else {
              //         panic!("continue outside of loop");
              //     }
              // }

            let _ = self.translate_expression(inst);
        }

        had_return
    }

    fn translate_loop(&mut self, b: &BlockExpr) {
        let loop_header = self.builder.create_block();
        let loop_bottom = self.builder.create_block();

        self.builder.ins().jump(loop_header, &[]);
        self.builder.switch_to_block(loop_header);

        self.loop_blocks.push(loop_bottom);

        let has_return = self.translate_block(b);

        if !has_return {
            self.builder.ins().jump(loop_header, &[]);
        }

        self.builder.seal_block(loop_header); // cannot seal block till we're done with the jump
                                              // expression
        self.builder.switch_to_block(loop_bottom);
        self.builder.seal_block(loop_bottom);

        self.loop_blocks.pop();
    }

    fn translate_call(&mut self, name: &str, args: &[Expression]) -> Value {
        let arg_vals: Vec<Value> = args
            .iter()
            .map(|expr| {
                self.translate_expression(expr)
                    .expect("expression should not be a statement")
            })
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
        let call_results = self.builder.inst_results(call);

        if call_results.is_empty() {
            // return a nullptr if the function returns nothing
            self.builder.ins().iconst(I64, 0)
        } else {
            self.builder.inst_results(call)[0]
        }
    }

    fn translate_record(
        &mut self,
        fields: &Vec<(String, Box<Expression>)>,
        record_type: &T,
    ) -> Value {
        let size = record_type.size_of();
        let data = StackSlotData::new(StackSlotKind::ExplicitSlot, size, 16); // TODO:
                                                                              // understand align_shift more
        let record_slot = self.builder.create_sized_stack_slot(data);
        let mut offset = 0;
        for (_, expr) in fields {
            let val = self
                .translate_expression(expr)
                .expect("expresssion should not be a statement");
            let expr_ty = expr.type_of(self.type_map);
            self.builder.ins().stack_store(val, record_slot, offset);

            let ty_size = expr_ty.size_of() as i32;
            // increment the offset by the size of the last type
            // ty_size needs to be converted to a i32 as the offset can be positive or negative
            offset += ty_size;
        }

        let addr = self.builder.ins().stack_addr(I64, record_slot, 0);
        addr
    }

    // come to think of it, an array is really just a record with offsets of the same size
    fn translate_array(&mut self, array: &Array) -> Value {
        let make_ctrl_array = self.module.declare_func_in_func(
            *self.ctx.function_ids.get("ctrl_make_array").unwrap(),
            self.builder.func,
        );

        let len = self.builder.ins().iconst(I32, array.size as i64);
        let element_size = self.builder.ins().iconst(I32, array.ty.size_of() as i64);
        let call = self
            .builder
            .ins()
            .call(make_ctrl_array, &[len, element_size]);

        let call_results = self.builder.inst_results(call);
        let array_ptr = call_results[0];

        let ty_size = array.ty.size_of() as i32;
        let mut offset = 32; // skip the length field
        for expr in array.elements.iter() {
            let val = self
                .translate_expression(expr)
                .expect("expresssion should not be a statement");

            self.builder
                .ins()
                .store(MemFlags::new(), val, array_ptr, offset);

            offset += ty_size;
        }

        array_ptr
    }

    // fn translate_for_in(&mut self, var_name: &str, in_expr: &Expression, loop_block: &BlockExpr) {
    //     let in_val = self.translate_expression(in_expr).unwrap();
    //
    //     // let T::Array(t) = in_expr.type_of(self.type_map) else {
    //     //     panic!("for in on non-array");
    //     // };
    //     let Expression::Array(a) = in_expr else {
    //         panic!("for in on non-array");
    //     };
    //
    //     let ty_size = a.ty.size_of();
    //
    //     let loop_header = self.builder.create_block();
    //     let loop_bottom = self.builder.create_block();
    //
    //     let index = self.builder.ins().iconst(I64, 0);
    //     let ty_size_val = self.builder.ins().iconst(I64, ty_size as i64);
    //
    //     let value = self.builder.ins().load(I64, MemFlags::new(), in_val, 0);
    //
    //     let loop_var = self.ctx.declare_variable(
    //         var_name,
    //         &mut self.builder,
    //         type_to_cranelift(&a.ty).expect("type"),
    //     );
    //     let index_var = self.ctx.declare_variable(
    //         "index",
    //         &mut self.builder,
    //         type_to_cranelift(&T::BuiltIn(BuiltinType::Int)).expect("type"),
    //     );
    //
    //     self.builder.switch_to_block(loop_header);
    //     self.builder.def_var(loop_var, value);
    //     self.builder.def_var(index_var, index);
    //
    //     let _ = self.translate_block(loop_block);
    //
    //     // have to jump back to the top of the loop after setting the value to the next element in
    //     // the array
    //
    //     let index = self.builder.use_var(index_var);
    //     let one = self.builder.ins().iconst(I64, 1);
    //     let new_index = self.builder.ins().iadd(index, one);
    //     self.builder.def_var(index_var, new_index);
    //
    //     let offset = self.builder.ins().imul(new_index, ty_size_val);
    //     let addr = self.builder.ins().iadd(in_val, offset);
    //     let new_val = self.builder.ins().load(I64, MemFlags::new(), addr, 0);
    //     self.builder.def_var(loop_var, new_val);
    //
    //     let should_break = self
    //         .builder
    //         .ins()
    //         .icmp_imm(IntCC::Equal, new_index, a.size as i64);
    //     self.builder
    //         .ins()
    //         .brif(should_break, loop_bottom, &[], loop_header, &[]);
    //     // self.builder.ins().jump(loop_header, &[]);
    //     self.builder.seal_block(loop_header);
    //
    //     self.builder.switch_to_block(loop_bottom);
    //     self.builder.seal_block(loop_bottom);
    // }

    fn translate_expression(&mut self, expr: &Expression) -> Option<Value> {
        match expr {
            Expression::Literal(literal) => Some(self.translate_literal(literal)),
            Expression::Assignment { ident, binding } => {
                let ty = match self.type_map.get(ident) {
                    Some(t) => t.clone(),
                    None => binding.type_of(self.type_map),
                };

                self.translate_assignment(ident, binding, &ty);
                // self.builder.ins().iconst(I64, 0) // placeholder nullptr
                None
            }
            Expression::Identifier(name) => {
                if let Some(var) = self.ctx.get_variable(name) {
                    Some(self.builder.use_var(var))
                } else {
                    panic!("undefined identifier {name}");
                }
            }
            Expression::Infix {
                finished,
                operation,
                lhs,
                rhs,
            } => {
                if !finished {
                    panic!("infix not finished");
                }
                Some(self.translate_infix(operation, lhs, rhs))
            }
            Expression::Return(expr) => {
                let return_val = self
                    .translate_expression(expr)
                    .expect("return value should be an expression, not a statement");
                self.builder.ins().return_(&[return_val]);

                None
            }
            Expression::Block(b) => {
                self.translate_block(b);
                // self.builder.ins().iconst(I64, 0) // placeholder nullptr
                None
            }
            Expression::Call(function_name, args) => Some(self.translate_call(function_name, args)),
            Expression::IfElse {
                cond,
                then_block,
                else_block,
            } => {
                self.translate_if_else(cond, then_block, else_block);
                // self.builder.ins().iconst(I64, 0) // placeholder nullptr
                None
            }
            Expression::RecordInitialization(record_name, fields) => {
                Some(self.translate_record(fields, self.type_map.get(record_name).unwrap()))
            }
            Expression::FieldAccess(record_name, field_name) => {
                let record_ptr = if let Some(var) = self.ctx.get_variable(record_name) {
                    self.builder.use_var(var)
                } else {
                    panic!("undefined identifier {record_name}");
                };

                let ty = self.type_map.get(record_name).unwrap();
                let (field_ty, field_offset) = ty.field_info(field_name);
                let val = self.builder.ins().load(
                    type_to_cranelift(field_ty).unwrap(),
                    MemFlags::new(),
                    record_ptr,
                    field_offset as i32,
                );

                Some(val)
            }
            Expression::Loop(b) => {
                self.translate_loop(b);
                // self.builder.ins().iconst(I64, 0) // placeholder nullptr
                None
            }
            // Expression::ForIn(var_name, in_expr, loop_block) => {
            //     self.translate_for_in(var_name, in_expr, loop_block);
            //     None
            // }
            Expression::Array(a) => Some(self.translate_array(a)),
            Expression::Index { array, index } => {
                let array_ptr = self
                    .translate_expression(array)
                    .expect("array should not be a statement");

                let index_val = self
                    .translate_expression(index)
                    .expect("index should not be a statement");

                let arr_ty = array.type_of(self.type_map);
                let elem_ty = arr_ty.final_ty();

                let ty_size = elem_ty.size_of() as i64;
                let offset = self.builder.ins().imul_imm(index_val, ty_size);

                // cast offset to i64
                let offset = self.builder.ins().sextend(I64, offset);
                let offset = self.builder.ins().iadd_imm(offset, 32); // skip the length field
                let addr = self.builder.ins().iadd(array_ptr, offset);
                let val = self.builder.ins().load(
                    type_to_cranelift(elem_ty).unwrap(),
                    MemFlags::new(),
                    addr,
                    0,
                );

                Some(val)
            }
            _ => unimplemented!(),
        }
    }
}

pub struct Compiler<'a> {
    ir: bool,
    module_name: &'a str,
    module: ObjectModule,
    function_ids: HashMap<String, FuncId>,
    strings: HashMap<String, GlobalValue>,
    type_map: &'a TypeMap,
}

impl<'a> Compiler<'a> {
    pub fn new(module_name: &'a str, ir: bool, type_map: &'a TypeMap) -> Self {
        let flags = settings::Flags::new(settings::builder());
        let isa_builder = cranelift_native::builder().expect("arch isnt supported");
        let isa = isa_builder.finish(flags).expect("isa builder not finished");

        let object_builder = ObjectBuilder::new(isa, "test", default_libcall_names())
            .expect("object builder not supported");

        let module = ObjectModule::new(object_builder);

        Self {
            ir,
            module_name,
            module,
            function_ids: HashMap::new(),
            strings: HashMap::new(),
            type_map,
        }
    }

    fn include_ctrl_stdlib(&mut self) -> Result<()> {
        let mut make_string = self.module.make_signature();
        make_string.params.push(AbiParam::new(I64)); // ptr to string
        make_string.params.push(AbiParam::new(I32)); // string len
        make_string.returns.push(AbiParam::new(I64)); // ptr to string_struct

        let make_string_id =
            self.module
                .declare_function("ctrl_make_string", Linkage::Import, &make_string)?;

        let mut make_array = self.module.make_signature();
        make_array.params.push(AbiParam::new(I32)); // array length
        make_array.params.push(AbiParam::new(I32)); // array element size
        make_array.returns.push(AbiParam::new(I64)); // ptr to array_struct

        let make_array_id =
            self.module
                .declare_function("ctrl_make_array", Linkage::Import, &make_array)?;

        let mut size_of = self.module.make_signature();
        size_of.params.push(AbiParam::new(I64)); // ptr to sizeable object
        size_of.returns.push(AbiParam::new(I32)); // length

        let size_of_id = self
            .module
            .declare_function("ctrl_size_of", Linkage::Import, &size_of)?;

        let mut print_string = self.module.make_signature();
        print_string.params.push(AbiParam::new(I64));

        let print_string_id =
            self.module
                .declare_function("print_string", Linkage::Import, &print_string)?;

        let mut print_int = self.module.make_signature();
        print_int.params.push(AbiParam::new(I32));

        let print_int_id =
            self.module
                .declare_function("print_int", Linkage::Import, &print_int)?;

        let mut concat_string = self.module.make_signature();
        concat_string.params.push(AbiParam::new(I64));
        concat_string.params.push(AbiParam::new(I64));
        concat_string.returns.push(AbiParam::new(I64));

        let concat_string_id =
            self.module
                .declare_function("ctrl_concat_string", Linkage::Import, &concat_string)?;

        self.function_ids
            .insert("ctrl_make_string".to_string(), make_string_id);
        self.function_ids
            .insert("ctrl_make_array".to_string(), make_array_id);
        self.function_ids
            .insert("ctrl_size_of".to_string(), size_of_id);
        self.function_ids
            .insert("ctrl_concat_string".to_string(), concat_string_id);
        self.function_ids
            .insert("print_string".to_string(), print_string_id);
        self.function_ids
            .insert("print_int".to_string(), print_int_id);

        Ok(())
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
        let mut ctx = Ctx::new(&self.function_ids, &mut self.strings);

        for (idx, (name, _)) in func.params.iter().enumerate() {
            let param_var = ctx.declare_variable(name, &mut builder, param_tys[idx]);
            let param_val = builder.block_params(block)[idx];
            builder.def_var(param_var, param_val);
        }

        let mut trans = Translator {
            builder,
            module: &mut self.module,
            ctx,
            type_map: self.type_map,
            loop_blocks: vec![],
        };

        for expr in &func.body.instructions {
            let _ = trans.translate_expression(expr);
        }

        if let Err(errors) = verify_function(trans.builder.func, trans.module.isa()) {
            eprintln!("Function verification failed:\n{}", errors);
            return Err(anyhow!("Function verification failed"));
        }

        if self.ir {
            println!("{}", trans.builder.func.display());
        }

        trans.builder.finalize();
        self.module.define_function(func_id, &mut func_ctx)?;

        Ok(())
    }

    // compilers are single use, this consumes itself after translating
    pub fn translate(mut self, ast: Vec<Expression>) -> Result<()> {
        self.include_ctrl_stdlib()?;

        for expr in ast {
            match expr {
                Expression::Function(func) => self.translate_function(&func)?,
                Expression::RecordDefinition(_) => {} // RecordDefinitions are type level and are not lowered to IR
                t => panic!("top level must be function or a type definition, got {t:?}"),
            }
        }

        let object = self.module.finish();
        let object_file = format!("{}.o", self.module_name);
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
