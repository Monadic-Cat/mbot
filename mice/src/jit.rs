use crate::parse::new::Program;

fn codegen(program: &Program) -> () {
    todo!("figure out how codegen with Cranelift works")
}

// Gonna be stuff related to doing PCG inside of generated code.
// Based on mostly hearsay, giving our codegen the opportunity
// to inline random number generation may be important.
// For now, though, I'm just dumping experiments in here.
fn pcg() {
    use ::cranelift::{codegen::{self, ir}, frontend};
    use codegen::settings::Configurable;
    use ::cranelift_jit::{JITBuilder, JITModule};
    use ::cranelift_module::{Module, Linkage};
    let mut flag_builder = codegen::settings::builder();
    // I don't know what this means. I just copied it from the cranelift-jit example.
    flag_builder.set("use_colocated_libcalls", "false").unwrap();
    // Apparently the example sets this to false because the x64 backend doesn't support it?
    flag_builder.set("is_pic", "false").unwrap();
    let isa_builder = ::cranelift_native::builder().unwrap_or_else(|msg| {
        panic!("host machine is not supported: {}", msg);
    });
    let isa = isa_builder.finish(codegen::settings::Flags::new(flag_builder));
    let ptr_type = ir::Type::triple_pointer_type(isa.triple());
    let mut module = JITModule::new(JITBuilder::with_isa(isa, ::cranelift_module::default_libcall_names()));

    let mut ctx = module.make_context();
    let mut func_ctx = frontend::FunctionBuilderContext::new();

    // Let's try and make pcg32_boundedrand_r.
    // Since our goal is currently just to enable faster plotting,
    // I'm willing to take a shortcut and compile in the initialized PRNG state.
    // So, let's see how this goes.
    let mut sig_rand = module.make_signature();
    sig_rand.params.push(ir::AbiParam::new(ptr_type));
    sig_rand.returns.push(ir::AbiParam::new(ir::types::I32));


    let mut sig_bounded_rand = module.make_signature();
    sig_bounded_rand.params.push(ir::AbiParam::new(ptr_type));
    sig_bounded_rand.params.push(ir::AbiParam::new(ir::types::I32));
    sig_bounded_rand.returns.push(ir::AbiParam::new(ir::types::I32));

    // We use local linkage because the only user of this function will be
    // other code compiled into the same module.
    let func_rand = module.declare_function("pcg32_random_r", Linkage::Local, &sig_rand).unwrap();
    let func_bounded_rand = module.declare_function("pcg32_boundedrand_r", Linkage::Local, &sig_bounded_rand).unwrap();

    ctx.func.signature = sig_rand;
    ctx.func.name = ir::ExternalName::user(0, func_rand.as_u32());
    {
        use frontend::FunctionBuilder;
        use ir::InstBuilder;
        let mut bcx: FunctionBuilder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
        let block = bcx.create_block();
        bcx.switch_to_block(block);
        bcx.append_block_params_for_function_params(block);
        let param = bcx.block_params(block)[0];
        // Note that making this load *trusted* tells the Cranelift optimizer that
        // this access is known to be aligned and non-trapping.
        // May cause wonky behavior if that assumption is wrong, so we need to ensure it's not.
        let oldstate = bcx.ins().load(ir::types::I64, ir::MemFlags::trusted(), param, 0);
        // I'm pretty sure a struct that's just two u64's will be 8 byte aligned,
        // and so they'll be directly adjacent to each other, so I should be able to
        // just load the next u64 in memory like this.
        let inc = bcx.ins().load(ir::types::I64, ir::MemFlags::trusted(), param, 8);
        // Unsigned Long Long (presumably u64) constant found in pcg32_random_r impl.
        let intermediate = bcx.ins().imul_imm(oldstate, 6364136223846793005);
        let new_state = bcx.ins().iadd(intermediate, inc);
        let _store_state = bcx.ins().store(ir::MemFlags::trusted(), new_state, param, 0);
        let intermediate = bcx.ins().ushr_imm(oldstate, 18);
        let intermediate = bcx.ins().bxor(intermediate, oldstate);
        let xorshifted = bcx.ins().ushr_imm(intermediate, 27);
        let rot = bcx.ins().ushr_imm(oldstate, 59);
        let intermediate_a = bcx.ins().ushr(xorshifted, rot);
        let intermediate_b = bcx.ins().ineg(rot);
        let intermediate_b = bcx.ins().band_imm(intermediate_b, 31);
        let intermediate_b = bcx.ins().ishl(xorshifted, intermediate_b);
        let intermediate = bcx.ins().bor(intermediate_a, intermediate_b);
        bcx.ins().return_(&[intermediate]);
        bcx.seal_all_blocks();
        bcx.finalize();
    }
    let mut trap_sink = codegen::binemit::NullTrapSink {};
    let mut stack_map_sink = codegen::binemit::NullStackMapSink {};
    module.define_function(func_rand, &mut ctx, &mut trap_sink, &mut stack_map_sink).unwrap();
    module.clear_context(&mut ctx);

    ctx.func.signature = sig_bounded_rand;
    ctx.func.name = ir::ExternalName::user(0, func_bounded_rand.as_u32());
    {
        use frontend::FunctionBuilder;
        use ir::InstBuilder;
        let mut bcx: FunctionBuilder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
        let block = bcx.create_block();
        bcx.switch_to_block(block);
        bcx.append_block_params_for_function_params(block);
        let local_func = module.declare_func_in_func(func_rand, &mut bcx.func);
        let rng_param = bcx.block_params(block)[0];
        let bound_param = bcx.block_params(block)[1];
        let threshold = bcx.ins().ineg(bound_param);
        // Note that this this requires we ensure the bound_param is not zero
        // that assurance is currently given by our disallowing d0s at parse time.
        let threshold = bcx.ins().urem(threshold, bound_param);
        // something something br_icmp
        // let cst = bcx.ins().
    }
    dbg!(ctx.func);
}
