use std::collections::HashMap;

use cranelift::codegen::ir::types::{Type, F64, I32, I64, I8};
use cranelift::prelude::AbiParam;
use cranelift_module::{FuncId, Linkage, Module};
use cranelift_object::ObjectModule;

use anyhow::Result;

pub fn include_ctrl_stdlib(
    module: &mut ObjectModule,
    function_ids: &mut HashMap<String, FuncId>,
) -> Result<()> {
    let mut make_string = module.make_signature();
    make_string.params.push(AbiParam::new(I64)); // ptr to string
    make_string.params.push(AbiParam::new(I32)); // string len
    make_string.returns.push(AbiParam::new(I64)); // ptr to string_struct

    let make_string_id =
        module.declare_function("ctrl_make_string", Linkage::Import, &make_string)?;

    let mut make_array = module.make_signature();
    make_array.params.push(AbiParam::new(I32)); // array length
    make_array.params.push(AbiParam::new(I32)); // array element size
    make_array.returns.push(AbiParam::new(I64)); // ptr to array_struct

    let make_array_id = module.declare_function("ctrl_make_array", Linkage::Import, &make_array)?;

    let mut size_of = module.make_signature();
    size_of.params.push(AbiParam::new(I64)); // ptr to sizeable object
    size_of.returns.push(AbiParam::new(I32)); // length

    let size_of_id = module.declare_function("ctrl_size_of", Linkage::Import, &size_of)?;

    let mut print_string = module.make_signature();
    print_string.params.push(AbiParam::new(I64));

    let print_string_id =
        module.declare_function("print_string", Linkage::Import, &print_string)?;

    let mut print_int = module.make_signature();
    print_int.params.push(AbiParam::new(I32));

    let print_int_id = module.declare_function("print_int", Linkage::Import, &print_int)?;

    let mut concat_string = module.make_signature();
    concat_string.params.push(AbiParam::new(I64));
    concat_string.params.push(AbiParam::new(I64));
    concat_string.returns.push(AbiParam::new(I64));

    let concat_string_id =
        module.declare_function("ctrl_concat_string", Linkage::Import, &concat_string)?;

    function_ids.insert("ctrl_make_string".to_string(), make_string_id);
    function_ids.insert("ctrl_make_array".to_string(), make_array_id);
    function_ids.insert("ctrl_size_of".to_string(), size_of_id);
    function_ids.insert("ctrl_concat_string".to_string(), concat_string_id);
    function_ids.insert("print_string".to_string(), print_string_id);
    function_ids.insert("print_int".to_string(), print_int_id);

    Ok(())
}
