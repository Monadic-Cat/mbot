mod mice;

#[proc_macro]
pub fn decl_ops(input: ::proc_macro::TokenStream) -> ::proc_macro::TokenStream {
    mice::decl_ops(input)
}
