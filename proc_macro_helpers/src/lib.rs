mod mice;
mod reloading;
use ::proc_macro::TokenStream;

#[proc_macro]
pub fn decl_ops(input: TokenStream) -> TokenStream {
    mice::decl_ops(input)
}

#[proc_macro_attribute]
pub fn reloadable(attr: TokenStream, item: TokenStream) -> TokenStream {
    reloading::reloadable(attr.into(), item.into()).into()
}
