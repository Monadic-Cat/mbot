#[cfg(any(feature = "ffi_internal", feature = "ffi_external"))]
pub mod ffi {
    use ::core::ops::{Deref, DerefMut};
    use ::core::ffi::c_void;
    use ::core::marker::PhantomData;
    // It wouldn't be a bad idea to pull in an FFI helper library,
    // instead of doing this ourselves.
    /// A FFI wrapper for the raw parts of a [`Vec<T>`](::std::vec::Vec).
    #[repr(C)]
    pub struct FfiVecU8<'a> {
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

    #[cfg(feature = "ffi_internal")]
    extern "C" fn free_ffi_vec(ptr: *mut u8, length: usize, capacity: usize) {
        let _ = unsafe { Vec::from_raw_parts(ptr, length, capacity) };
    }

    // The stuff in this impl block is only allowed on this side of the FFI.
    // It's private, and cfg-ed out for external users.
    #[cfg(feature = "ffi_internal")]
    impl<'a> FfiVecU8<'a> {
        fn from_vec(mut vec: Vec<u8>) -> FfiVecU8<'static> {
            // Correctness (and Safety in free_ffi_vec): This is how std implements Vec::into_raw_parts,
            // so I'm guessing the order used doesn't mess with provenance.
            let (ptr, length, capacity) = (vec.as_mut_ptr(), vec.len(), vec.capacity());
            ::core::mem::forget(vec);
            FfiVecU8 {
                ptr, length, capacity,
                free_function: free_ffi_vec,
                _free_function_lifetime: PhantomData
            }
        }
        // // Only allowed on this side of the FFI.
        // fn into_vec(Self { ptr, length, capacity, .. }: Self) -> Vec<u8> {
        //     unsafe { Vec::from_raw_parts(ptr, length, capacity) }
        // }
    }
    impl<'a> Drop for FfiVecU8<'a> {
        fn drop(&mut self) {
            // Safety: Just calling vector drop via FFI.
            // This passes back the data we were given via the same FFI.
            #[allow(unused_unsafe)]
            unsafe { (self.free_function)(self.ptr, self.length, self.capacity); }
        }
    }
    impl<'a> Deref for FfiVecU8<'a> {
        type Target = [u8];
        fn deref(&self) -> &Self::Target {
            // Safety: ptr and length are guaranteed to be valid and correct, by construction of FfiVecU8.
            unsafe { ::core::slice::from_raw_parts(self.ptr, self.length) }
        }
    }
    impl<'a> DerefMut for FfiVecU8<'a> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            // Safety: See Deref impl above.
            unsafe { ::core::slice::from_raw_parts_mut(self.ptr, self.length) }
        }
    }
    unsafe impl<'a> Send for FfiVecU8<'a> {}
    unsafe impl<'a> Sync for FfiVecU8<'a> {}

    /// Owning pointer to prepared dice program.
    #[derive(Debug)]
    #[repr(transparent)]
    pub struct Prepared<'a> {
        /// Type erased owning pointer to prepared dice program.
        #[cfg_attr(not(feature = "actual_plotter"), allow(dead_code))]
        ptr: *mut c_void,
        // Inside the dynamic module, this lifetime is essentially forever.
        // It's only on the other side of the FFI that this lifetime becomes relevant.
        _guard_lifetime: PhantomData<&'a ()>,
    }
    unsafe impl<'a> Send for Prepared<'a> {}

    #[cfg(feature = "ffi_internal")]
    impl Prepared<'static> {
        // This is an inherent impl to avoid accidental leakage.
        fn from<T>(val: T) -> Self {
            let boxed = Box::new(val);
            Self {
                ptr: Box::into_raw(boxed).cast(),
                _guard_lifetime: PhantomData,
            }
        }
        /// # Safety
        /// The [`Prepared`] must have been produced from a value of type `T`.
        unsafe fn into<T>(self) -> T {
            let Self { ptr, .. } = self;
            #[allow(unused_unsafe)]
            unsafe { *Box::from_raw(ptr.cast::<T>()) }
        }
    }

    // TODO: consider using the abi_stable crate for FFI safe non exhaustive enums.
    #[repr(u8, C)]
    pub enum PrepRet<'a> {
        Ok(Prepared<'a>),
        InvalidExpression,
        TooExpensive,
    }

    #[repr(u8, C)]
    pub enum DrawRet<'a> {
        Ok(FfiVecU8<'a>),
        OverflowPositive,
        OverflowNegative,
    }

    /// # Safety
    /// Must be called with a valid pointer and length for a UTF-8 slice
    #[cfg(feature = "ffi_internal")]
    #[no_mangle]
    pub unsafe extern "C" fn prep_expr(expression: *const u8, length: usize) -> PrepRet<'static> {
        #![allow(unused_unsafe)]
        use crate::plot_impl;
        // Safety: This is given to us from a &str on the other side of the FFI.
        let expression = unsafe { ::core::slice::from_raw_parts(expression, length) };
        // Safety: This is given to us from a &str on the other side of the FFI.
        let expression = unsafe { ::core::str::from_utf8_unchecked(expression) };
        match plot_impl::prepare_expression(expression) {
            Ok(prepared) => PrepRet::Ok(Prepared::from(prepared)),
            Err(plot_impl::PreparationError::TooExpensive) => PrepRet::TooExpensive,
            Err(plot_impl::PreparationError::InvalidExpression) => PrepRet::InvalidExpression,
        }
    }

    #[cfg(feature = "ffi_internal")]
    #[no_mangle]
    pub extern "C" fn draw_impl(prepared: Prepared<'static>) -> DrawRet<'static> {
        use crate::plot_impl;
        // Safety: This is the same type as provided by `prep_expr`,
        // and the other side of the FFI is obligated to give us what we hand out.
        let expression: plot_impl::PreparedProgram = unsafe { prepared.into() };
        use ::mice::prelude::MiceError;
        match plot_impl::draw(&expression) {
            Ok(buffer) => DrawRet::Ok(FfiVecU8::from_vec(buffer)),
            Err(MiceError::OverflowPositive(_)) => DrawRet::OverflowPositive,
            Err(MiceError::OverflowNegative(_)) => DrawRet::OverflowNegative,
            Err(MiceError::InvalidExpression(_)) => unreachable!("we check for this inside prep_expr"),
            Err(MiceError::InvalidDie) => unreachable!("the current mice parser cannot produce dice with negative sides"),
        }
    }
}


#[cfg(feature = "actual_plotter")]
pub mod plot_impl {
    use ::mice::prelude::MiceError;
    use ::std::collections::HashMap;
    use ::core::iter;
    use ::plotters::prelude::*;
    use ::plotters_bitmap::BitMapBackend;
    use ::plotters_bitmap::bitmap_pixel::{RGBPixel, PixelFormat};
    use ::plotters::data::fitting_range;

    const RESOLUTION: (u32, u32) = (600, 600);
    const BUFFER_WIDTH: usize = (RESOLUTION.0 * RESOLUTION.1) as usize * <RGBPixel as PixelFormat>::PIXEL_SIZE;
    const SAMPLE_SIZE: usize = 1_000_000;

    #[derive(Debug)]
    pub struct PreparedProgram {
        expression: ::mice::parse::Expression,
        // We heap allocate here because the input message string slice is not enforced to live
        // as long as this loaded module. (Nor would it make sense for it to be.)
        caption: String,
    }

    #[derive(Debug)]
    pub enum PreparationError {
        InvalidExpression,
        TooExpensive,
    }

    pub fn prepare_expression(expression: &str) -> Result<PreparedProgram, PreparationError> {
        let caption = String::from(expression);
        match ::mice::parse::dice(expression) {
            Ok((input, Ok(expression))) if input.is_empty() => {
                use ::mice::util::ExpressionExt;
                if !expression.exceeds_cap(200) {
                    Ok(PreparedProgram { expression, caption })
                } else {
                    Err(PreparationError::TooExpensive)
                }
            },
            _ => Err(PreparationError::InvalidExpression),
        }
    }

    pub fn draw(exp: &PreparedProgram) -> Result<Vec<u8>, MiceError> {
        let PreparedProgram { expression, caption } = exp;
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
        use ::mice::stack::Overflow;
        // Note: This is guaranteed not to fail, since parse_expression accepts
        // a superset of the language Expression accepts.
        let program = ::mice::parse::new::parse_expression(caption.as_bytes()).unwrap().1;
        let stack_program = ::mice::stack::compile(&program);
        let mut machine = ::mice::stack::Machine::new();
        let sample = iter::repeat_with(|| machine.eval_with(&mut rng, &stack_program))
            .inspect(|x| x.iter().for_each(|x| *counts.entry(*x).or_insert(0) += 1))
            .take(SAMPLE_SIZE).collect::<Result<Vec<_>, _>>().map_err(|e| match e {
                Overflow::Positive => MiceError::OverflowPositive(::mice::OverflowPositive),
                Overflow::Negative => MiceError::OverflowNegative(::mice::OverflowNegative),
            })?;
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
        Ok(image)
    }
}
