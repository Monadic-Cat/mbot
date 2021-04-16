//! Graphing distributions for dice expressions.
//! I didn't feel like doing much math for this one,
//! so, as a preliminary attempt, we just take a ton
//! of samples and chart them.
//! Because of this, we should consider limiting the
//! size of input dice expressions more harshly.

// Important Note: This feature currently does not even attempt to support
// more than Linux (glibc) on x86_64 and armhf.
// In the future, I intend to make it possible to statically compile in
// the exact code being loaded dynamically here, so we use the same plotting
// code regardless of whether we're able to dynamically load it.
#[cfg(feature = "reloadable_plotter")]
pub mod reloadable {
    use ::core::ffi::c_void;
    use ::std::os::raw::c_char;
    use ::libc::c_int;
    use ::once_cell::sync::Lazy;
    use ::parking_lot::{RwLock, RwLockReadGuard};
    use ::reloadable_plotter::{FfiVecU8, Prepared, PrepRet, DrawRet};


    #[derive(Debug)]
    pub enum PreparationError {
        InvalidExpression,
        TooExpensive,
    }
    #[derive(Debug)]
    pub enum Overflow {
        Positive,
        Negative,
    }

    // # Soundness Where libloading Failed! OwO
    // (truthfully, I'm taking a performance cut because I know I can;
    // it's not fair to say this is universally a better solution)
    // TODO: examine all the phantom lifetimes introduced for correct variance
    // IMPORTANT NOTE: This MUST NOT live longer than the dynamic module it uses.
    // Nor can the closures it holds references to.
    pub struct PlotGuard<'a> {
        module: RwLockReadGuard<'a, Option<Plotter>>,
    }
    impl<'a> PlotGuard<'a> {
        pub fn prep(&self, expression: &str) -> Result<Prepared<'a>, PreparationError> {
            (self.module.as_ref().unwrap().prep)(self, expression)
        }
        pub fn draw(&self, prepared: Prepared<'a>) -> Result<FfiVecU8<'a>, Overflow> {
            (self.module.as_ref().unwrap().draw)(self, prepared)
        }
    }

    #[derive(Copy, Clone)]
    #[repr(transparent)]
    struct LibraryHandle {
        _ptr: *const c_void,
    }
    
    /// Owning container for the dynamically loaded plotting module.
    /// Only one will ever exist at a time.
    pub struct Plotter {
        // Dynamic dispatch doesn't really matter here,
        // since these are each called only once per command invocation.
        // Note that, for soundness, these trait objects MUST NOT implement Copy or Clone.
        // The idea here is to avoid letting a user clone function pointers obtained
        // via this API, by simply not exposing references to those function pointers.
        // That way, they can't escape the module lifetime bound, expressed via PlotGuard<'a>.
        /// Parse and otherwise prepare a dice expression for execution.
        pub prep: Box<dyn for<'a> Fn(&PlotGuard<'a>, &str) -> Result<Prepared<'a>, PreparationError> + Send + Sync>,
        pub draw: Box<dyn for<'a> Fn(&PlotGuard<'a>, Prepared<'a>) -> Result<FfiVecU8<'a>, Overflow> + Send + Sync>,
        handle: LibraryHandle,
        // Prevent external construction.
        _priv: (),
    }

    // Safety: Uh... `dlopen` and `dlclose` are 'MT-Safe', so I'm gonna cross my fingers and hope here.
    // In seriousness, we guard using those appropriately with the RwLock.
    unsafe impl Send for Plotter {}
    unsafe impl Sync for Plotter {}

    use ::core::sync::atomic::{AtomicBool, Ordering};
    // Guard against multiple loading.
    static FIRST_LOADED: AtomicBool = AtomicBool::new(false);
    // TODO: I may prefer a lock that strongly prioritizes writers.
    // In any case, I *do* want a way to force through fixes for vulnerabilities.
    static PLOTTER: Lazy<RwLock<Option<Plotter>>> = Lazy::new(|| {
        // Safety: This is the only place this function is allowed to be called.
        RwLock::new(Some(unsafe { Plotter::load_first() }))
    });

    impl Plotter {
        /// # Safety
        /// This function must be called at most once between [`unload`](Self::unload) calls.
        /// Since this is global state, prefer [`load_first`](Self::load_first) and
        /// [`reload`](Self::reload).
        unsafe fn load() -> Plotter {
            #![allow(unused_unsafe)]
            /// C string literal.
            macro_rules! cs {
                ($text:literal) => {
                    concat!($text, "\0").as_bytes().as_ptr().cast::<c_char>()
                };
            }
            extern "C" {
                // We could use the libc crate provided binding for this, but I don't wanna.
                fn dlopen(filename: *const c_char, flags: c_int) -> LibraryHandle;
                // TODO: consider using versioned symbols?
                fn dlsym(handle: LibraryHandle, symbol: *const c_char) -> *const c_void;
            }
            // TODO: consider parameterizing the filename of the shared object somehow
            // Safety: lol idk
            let handle = unsafe { dlopen(cs!("libreloadable_plotter.so"), ::libc::RTLD_LAZY) };
            // The lifetimes on these are wrong, but the correct ones can't be named until later.
            let prep_expr: extern "C" fn(*const u8, usize) -> PrepRet<'static>
                = unsafe { ::core::mem::transmute(dlsym(handle, cs!("prep_expr"))) };
            let draw_impl: extern "C" fn(Prepared) -> DrawRet<'static> = unsafe { ::core::mem::transmute(dlsym(handle, cs!("draw_impl"))) };
            Plotter {
                prep: Box::new(move |_guard, expression| {
                    let len = expression.len();
                    // Safety: FFI.
                    match unsafe { prep_expr(expression.as_bytes().as_ptr(), len) } {
                        PrepRet::Ok(x) => Ok(x),
                        PrepRet::InvalidExpression => Err(PreparationError::InvalidExpression),
                        PrepRet::TooExpensive => Err(PreparationError::TooExpensive),
                    }
                }),
                // Safety: Note that generally invariants that are enforced by construction
                // will only hold inside of the lifetime of a dynamically loaded module.
                draw: Box::new(move |_guard, prepared| {
                    // Safety: This is correct by construction of a `Prepared` instance.
                    // Note that the dynamic module handles freeing the memory
                    // backing a `Prepared`, so it would be incorrect for `Prepared` to impl Copy,
                    // or for this method to be called by reference.
                    match unsafe { draw_impl(prepared) } {
                        DrawRet::Ok(buffer) => Ok(buffer),
                        DrawRet::OverflowPositive => Err(Overflow::Positive),
                        DrawRet::OverflowNegative => Err(Overflow::Negative),
                    }
                }),
                _priv: (),
                handle,
            }
        }

        /// # Safety
        /// This function may only be called inside the lazy initializer for [`PLOTTER`].
        unsafe fn load_first() -> Plotter {
            assert!(FIRST_LOADED.compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst).is_ok());
            // Safety: This is 
            #[allow(unused_unsafe)]
            unsafe { Self::load() }
        }

        // TODO: decide how best to make these work with async
        /// Ensure the plotting module is loaded and lock it for use.
        #[allow(dead_code)]
        pub fn lock<'a>() -> PlotGuard<'a> {
            let guard = PLOTTER.read();
            // TODO: decide whether to put this check in the return type,
            // instead of panicking
            match *guard {
                Some(_) => (),
                None => panic!("module not loaded"),
            }
            PlotGuard { module: guard }
        }
        /// Wait for all users of the plotting module to finish,
        /// then reload it.
        pub fn reload() {
            extern "C" {
                fn dlclose(handle: LibraryHandle) -> c_int;
            }
            let mut guard = PLOTTER.write();
            match guard.take() {
                Some(plotter) => unsafe { dlclose(plotter.handle); },
                None => (),
            }
            *guard = unsafe { Some(Self::load()) };
        }
        #[allow(dead_code)]
        pub fn unload() {
            extern "C" {
                fn dlclose(handle: LibraryHandle) -> c_int;
            }
            let mut guard = PLOTTER.write();
            let plotter = guard.take();
            // TODO: consider making unload idempotent
            // Safety: This is the handle we got from dlopen above, so it should be valid.
            unsafe { dlclose(plotter.unwrap().handle) };
        }
    }
}
