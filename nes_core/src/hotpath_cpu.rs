//! CPU-sampling (samply) attribution support for the hotpath instrumentation.
//!
//! `#[hotpath::measure(impl_type = "…")]` registers the name
//! `module_path::Type::fn` (what <https://hotpath.rs/cpu_profiling> documents),
//! but the demangled symbol of a *generic* inherent impl keeps its type
//! arguments (`Type<…>::fn`). hotpath 0.30.2 matches a sampled address either
//! exactly, or by the longest registered prefix followed by `::`, so the `<…>`
//! breaks the prefix and every sample is dropped — the report prints "no
//! samples attributed to instrumented functions".
//!
//! `std::any::type_name` spells the same type arguments the demangler prints,
//! so [`hotpath_cpu_alias!`] aliases the registered name to the real symbol of
//! one concrete `Self`. hotpath keeps one symbol per alias, so a label
//! measured through several instantiations (one `NesMachine<…>` per renderer,
//! say) is attributed for whichever instantiation runs first.
//!
//! Nothing here compiles without the `hotpath-cpu` feature; remove it once
//! hotpath matches generic impl symbols itself.

/// Alias `label` (the name `#[hotpath::measure]` registered) to `T`'s real
/// symbol, so samply samples resolve back to the instrumented function.
#[cfg(feature = "hotpath-cpu")]
pub(crate) fn register_cpu_symbol_alias<T>(label: &'static str, fn_name: &str) {
    let symbol: &'static str =
        Box::leak(format!("{}::{fn_name}", std::any::type_name::<T>()).into_boxed_str());
    hotpath::functions::register_cpu_label_alias(label, symbol);
}

/// Point CPU attribution at `$ty`'s `$fn_name` symbol, once per call site.
///
/// `$label` must be the exact name the enclosing `#[hotpath::measure]`
/// registered, typically `concat!(module_path!(), "::Type::fn")`.
macro_rules! hotpath_cpu_alias {
    ($ty:ty, $label:expr, $fn_name:literal) => {
        #[cfg(feature = "hotpath-cpu")]
        {
            static ALIAS_REGISTERED: ::std::sync::Once = ::std::sync::Once::new();
            ALIAS_REGISTERED.call_once(|| {
                $crate::hotpath_cpu::register_cpu_symbol_alias::<$ty>($label, $fn_name)
            });
        }
    };
}

pub(crate) use hotpath_cpu_alias;
