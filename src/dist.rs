//! Graphing distributions for dice expressions.
//! I didn't feel like doing much math for this one,
//! so, as a preliminary attempt, we just take a ton
//! of samples and chart them.
//! Because of this, we should consider limiting the
//! size of input dice expressions more harshly.

use ::plotters::prelude::*;
use ::plotters_bitmap::BitMapBackend;
use ::plotters_bitmap::bitmap_pixel::{RGBPixel, PixelFormat};
use ::plotters::data::fitting_range;
use ::mice::parse::Expression;
use ::mice::prelude::MiceError;
use ::core::iter;
use ::std::collections::HashMap;

const RESOLUTION: (u32, u32) = (600, 600);
const BUFFER_WIDTH: usize = (RESOLUTION.0 * RESOLUTION.1) as usize * <RGBPixel as PixelFormat>::PIXEL_SIZE;
const SAMPLE_SIZE: usize = 1_000_000;

// Important Note: This feature currently does not even attempt to support
// more than Linux (glibc) on x86_64 and armhf.
// In the future, I intend to make it possible to statically compile in
// the exact code being loaded dynamically here, so we use the same plotting
// code regardless of whether we're able to dynamically load it.
#[cfg(feature = "reloadable_plotter")]
pub mod reloadable {
    use ::core::ops::{Deref, DerefMut};
    use ::core::ffi::c_void;
    use ::std::os::raw::c_char;
    use ::libc::c_int;
    use ::core::marker::PhantomData;
    use ::once_cell::sync::Lazy;
    use ::std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

    #[derive(Debug)]
    pub enum PreparationError {
        InvalidExpression,
        TooExpensive,
    }
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
        pub fn draw(&self, prepared: Prepared<'a>) -> Result<FFIVecU8<'a>, Overflow> {
            (self.module.as_ref().unwrap().draw)(self, prepared)
        }
    }

    #[derive(Copy, Clone)]
    #[repr(transparent)]
    struct LibraryHandle {
        ptr: *const c_void,
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
        pub draw: Box<dyn for<'a> Fn(&PlotGuard<'a>, Prepared<'a>) -> Result<FFIVecU8<'a>, Overflow> + Send + Sync>,
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
    #[allow(dead_code)]
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
            // TODO: decide whether to propagate poisoning
            let guard = PLOTTER.read().unwrap();
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
            let _guard = PLOTTER.write();
            todo!("hot reloading")
        }
        pub fn unload() {
            let _guard = PLOTTER.write();
            todo!("module unloading")
        }
    }

    // It wouldn't be a bad idea to pull in an FFI helper library,
    // instead of doing this ourselves.
    /// A FFI wrapper for the raw parts of a [`Vec<T>`](::std::vec::Vec).
    #[repr(C)]
    pub struct FFIVecU8<'a> {
        ptr: *mut u8,
        length: usize,
        capacity: usize,
        // Since we want to free this using whatever allocator the
        // dynamically loaded module used to allocate it,
        // we need to make sure we don't keep it around longer than
        // that module, so we can pass it back.
        // TODO: determine if this needs to instead hold a handle
        // to more stuff.
        free_function: extern "C" fn(*mut u8, usize, usize),
        _free_function_lifetime: PhantomData<&'a dyn Fn(*mut u8, usize, usize)>,
    }
    impl<'a> Drop for FFIVecU8<'a> {
        fn drop(&mut self) {
            // Safety: Just calling vector drop via FFI.
            // This passes back the data we were given via the same FFI.
            #[allow(unused_unsafe)]
            unsafe { (self.free_function)(self.ptr, self.length, self.capacity); }
        }
    }
    impl<'a> Deref for FFIVecU8<'a> {
        type Target = [u8];
        fn deref(&self) -> &Self::Target {
            // Safety: ptr and length are guaranteed to be valid and correct, by construction of FFIVecU8.
            unsafe { ::core::slice::from_raw_parts(self.ptr, self.length) }
        }
    }
    impl<'a> DerefMut for FFIVecU8<'a> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            // Safety: See Deref impl above.
            unsafe { ::core::slice::from_raw_parts_mut(self.ptr, self.length) }
        }
    }

    #[repr(u8, C)]
    pub enum DrawRet<'a> {
        Ok(FFIVecU8<'a>),
        OverflowPositive,
        OverflowNegative,
    }
    #[repr(u8, C)]
    pub enum PrepRet<'a> {
        Ok(Prepared<'a>),
        InvalidExpression,
        TooExpensive,
    }

    /// Owning pointer to prepared dice program.
    #[derive(Debug)]
    #[repr(transparent)]
    pub struct Prepared<'a> {
        /// Type erased owning pointer to prepared dice program.
        _ptr: *mut c_void,
        _guard_lifetime: PhantomData<&'a ()>,
    }
    // // Note that there are very few proof obligations here.
    // extern "C" {
    //     fn prep_expr(expression: *const u8, length: usize) -> PrepRet<'static>;
    //     fn draw_impl(prepared: Prepared) -> DrawRet<'static>;
    //     /// Frees the buffer returned by [`draw_impl`].
    //     fn free_buf(ptr: *mut u8, length: usize, capacity: usize);
    // }
    pub enum DiceError {
        InvalidExpression,
        OverflowPositive,
        OverflowNegative,
    }
    // #[allow(dead_code)]
    // pub fn draw(expression: &str) -> Result<FFIVecU8, DiceError> {
    //     let len = expression.len();
    //     let ptr = expression.as_ptr();
    //     // Safety: ptr refers to a valid string buffer, and len is correct for that buffer.
    //     let prepared = match unsafe { prep_expr(ptr, len) } {
    //         PrepRet::Ok(x) => x,
    //         PrepRet::InvalidExpression => return Err(DiceError::InvalidExpression),
    //     };
    //     match unsafe { draw_impl(prepared) } {
    //         DrawRet::Ok(x) => Ok(x),
    //         DrawRet::InvalidExpression => Err(DiceError::InvalidExpression),
    //         DrawRet::OverflowPositive => Err(DiceError::OverflowPositive),
    //         DrawRet::OverflowNegative => Err(DiceError::OverflowNegative),
    //     }
    // }
}

// TODO: adjust sampling for larger inputs?
pub(crate) fn draw(exp: &Expression, caption: &str) -> Result<Box<[u8]>, MiceError> {
    // This is FAR too expensive computationally right now.
    let mut counts = HashMap::new();
    let mut rng = {
        use ::rand::SeedableRng;
        ::rand::rngs::SmallRng::from_entropy()
    };
    // TODO: prove no overflow
    // TODO: compute range analytically
    // explosions will be annoying here.
    // perhaps we should enable more precise handling and higher caps
    // when misbehaving things like explosions aren't present.
    let sample = iter::repeat_with(|| exp.roll_with(&mut rng).map(|x| x.total()))
        .inspect(|x| x.iter().for_each(|x| *counts.entry(*x).or_insert(0) += 1))
        .take(SAMPLE_SIZE).collect::<Result<Vec<_>, _>>()?;
    let max = *counts.values().max().unwrap() * 4 / 3;
    // Note: We could *definitely* reuse this buffer.
    let mut buffer = vec!(0u8; BUFFER_WIDTH);
    // ## Plot a Histogram
    {
        let root = BitMapBackend::with_buffer(&mut buffer, RESOLUTION);
        let area = root.into_drawing_area();
        area.fill(&WHITE).unwrap();
        let mut chart = ChartBuilder::on(&area)
            .caption(caption, ("sans-serif", 50).into_font())
            .margin(5)
            .x_label_area_size(30)
            .y_label_area_size(30)
            .build_cartesian_2d({
                let mut domain = fitting_range(&sample);
                domain.end += 1;
                domain
            }, 0i64..max).unwrap();
        chart.configure_mesh().y_label_formatter(&|y| format!("{:.0}%", (*y as f64) * 100.0 / SAMPLE_SIZE as f64)).draw().unwrap();
        chart.draw_series(Histogram::vertical(&chart)
                          .style(RED.mix(0.5).filled())
                          .data(sample.iter().map(|x| (*x as i32, 1)))).unwrap();
    }
    // ## Encode as PNG
    let mut image = Vec::new();
    {
        let mut encoder = ::png::Encoder::new(&mut image, RESOLUTION.0, RESOLUTION.1);
        encoder.set_color(::png::ColorType::RGB);
        encoder.set_depth(::png::BitDepth::Eight);
        let mut writer = encoder.write_header().unwrap();
        writer.write_image_data(&buffer).unwrap();
    }
    Ok(image.into_boxed_slice())
}
