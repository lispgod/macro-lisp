use proc_macro2::TokenStream as TokenStream2;

// ─── Debug: pretty-print generated code when `debug-expansion` feature is enabled ───
#[cfg(feature = "debug-expansion")]
pub(crate) fn debug_expansion(label: &str, output: &TokenStream2) {
    if let Ok(file) = syn::parse_file(&output.to_string()) {
        eprintln!(
            "=== {} expansion ===\n{}",
            label,
            prettyplease::unparse(&file)
        );
    }
}

#[cfg(not(feature = "debug-expansion"))]
pub(crate) fn debug_expansion(_label: &str, _output: &TokenStream2) {}
