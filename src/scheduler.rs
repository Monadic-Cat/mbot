// Security: The goal is to allow untrusted WASM scheduler plugins to run
// inside the server process. But, I am not currently confident in its ability to do this securely.
use ::wasmtime::*;

/// Address of a location in WebAssembly memory,
/// or size of an object in WebAssembly memory.
type WSize = u32;

/// Type erased DST pointer to WebAssembly memory.
/// This is returned by our loaded WASM modules.
#[derive(Debug)]
#[repr(C)]
struct FatPointer {
    size: WSize,
    ptr: WSize,
}

/// A marker trait for *plain old data* types.
/// An implementor of this trait must guarantee that
/// all bitpatterns of its size are valid instances of it.
unsafe trait Pod {}
macro_rules! unsafe_pod_impls {
    ($($t:ty)*) => {
        $(
            unsafe impl Pod for $t {}
        )*
    }
}
unsafe_pod_impls! { i8 i16 i32 i64
u8 u16 u32 u64
FatPointer ThinPointer }

/// Type erased thin pointer to WebAssembly memory.
#[derive(Copy, Clone, Debug)]
#[repr(transparent)]
struct ThinPointer(WSize);
impl ThinPointer {
    /// Bounds checked read of a pointer received from WASM.
    /// Seeing as we don't trust the WASM we're loading ultimately,
    /// this is necessary for soundness.
    fn read<T: Pod>(&self, memory: &Memory) -> Result<T, ()> {
        if memory.data_size() > (self.0 as usize + ::core::mem::size_of::<T>()) {
            #[allow(unused_unsafe)]
            Ok(unsafe {
                // This needs to tolerate unaligned reads, because
                // the WASM plugin may intentionally give us misaligned data
                // to cause misbehavior if we don't handle it right.
                ::core::ptr::read_unaligned(memory.data_ptr().offset(self.0 as isize).cast())
            })
        } else {
            Err(())
        }
    }
}

#[cfg(test)]
#[test]
fn load_wasm() {
    // TODO: limit requested stack size
    // TODO: limit requested heap size
    // TODO: provide and limit heap growth
    let wasm = ::std::fs::read(concat!(env!("CARGO_MANIFEST_DIR"), "/plugins/masks.wasm"))
        .expect("couldn't read input wasm file");
    let store = Store::default();
    let module = Module::new(store.engine(), wasm).expect("couldn't create wasm module");

    let host_print = Func::wrap(&store, |caller: Caller, param: i32| {
        let memory = match caller
            .get_export("memory")
            .expect("all modules we instantiate should have memory")
        {
            Extern::Memory(mem) => mem,
            _ => panic!("export `memory` should be a Memory"),
        };
        let size = memory.data_size();
        if size > param as usize {
            let mut stdout = ::std::io::stdout();
            let base_ptr = memory.data_ptr();
            let mut offset_ptr = unsafe { base_ptr.offset(param as isize) };
            loop {
                if unsafe { offset_ptr.offset_from(base_ptr) } > size as isize {
                    break
                } else {
                    // Safety: Byte reads are always aligned.
                    let byte: u8 = unsafe { offset_ptr.read() };
                    if byte == 0 {
                        break
                    } else {
                        use ::std::io::Write;
                        // If writing to stdout fails, I don't care.
                        // This debug printing routine is here as a convenience.
                        let _ = stdout.write(&[byte]);
                    }
                }
                offset_ptr = unsafe { offset_ptr.offset(1) };
            }
            0
        } else {
            eprintln!("Invalid print call: {}", param);
            -1
        }
    });

    let instance = Instance::new(&store, &module, &[host_print.into()])
        .expect("couldn't instantiate wasm module");
    let memory = instance.get_memory("memory").unwrap();
    let next_player = instance.get_func("next_player").unwrap().get1().unwrap();
    let initialize = instance.get_func("initialize").unwrap().get0().unwrap();
    let state_fptr_ptr = ThinPointer(initialize().unwrap());
    let state_fptr: FatPointer = state_fptr_ptr.read(&memory).unwrap();
    println!("fptr: {:?}", state_fptr);
    let result: Result<u64, _> = next_player(state_fptr.ptr);
    println!("Result: {:?}", result);
}
