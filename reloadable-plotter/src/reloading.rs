//! Some proper support for runtime reloadable shared libraries.
//! I can't paper over the existence of FFI, but I can generate
//! module guard types and automatically prevent data races on the code being reloaded.

/// Declare a runtime reloadable module.
#[macro_export]
macro_rules! decl_module {
    ($($t:tt)*) => {
        // TODO: permit cfg-ing the decision
        // to link statically or at runtime.
    }
}

// Like decl_module, but does more work?
// Like, might handle FFI related type declarations?
// Point is that decl_module is gonna be somewhat barebones and
// we may be able to abstract away more.
macro_rules! link {
    ($($t:tt)*) => {}
}

decl_module! {
    // Associate feature gates with runtime reloading functionality and what not.
    if #[cfg(all(feature = "reloadable_plotter", feature = "static_plotter"))] {
        compile_error!("The reloadable_plotter and static_plotter features are mutually exclusive. You must select either one or the other, not both.");
    } else if #[cfg(feature = "reloadable_plotter")] {
        reloadable("libreloadable_plotter.so")
    } else if #[cfg(feature = "static_plotter")] {
        static
    } else {
        compile_error!("To enable dice plotting, you must select either the reloadable_plotter or static_plotter feature.");
    }
    // Name module, special module guard lifetime, guard, and guard type.
    let guard: PlotGuard<'module> = Plot::lock(&'module self);
    // RStr is a FFI safe equivalent of &str.
    // We introduce a special '=>' operator to indicate automatic conversion for FFI.
    fn prep(&guard, expression: &str => RStr<'_>) -> PrepRet<'module> => Result<Prepared<'module>, PreparationError>;
    fn draw(&guard, prepared: Prepared<'module>) -> DrawRet<'module> => Result<FfiVecU8<'a>, Overflow>;
}

// TODO: come up with an interface that's compatible with preemption
// for high priority patches.
// Maybe I can use POSIX real time signals for that?
// I'd eventually need to find a way to implement it on Windows, though.
// I may also be able to support limited preemption in the form of
// integration with an async runtime.
// The easiest thing I can do in this direction is make a locking mechanism with
// multiple priorities and differing exclusivity. The current setup uses an RwLock
// where users of the module are readers and reloading takes a writer lock.
// Note: For my current purposes (my instance of mbot), I'm fine with killing the process
// if I think anything nefarious is going on *while* I'm deploying a fix.
// So, this isn't exactly high priority for me.
