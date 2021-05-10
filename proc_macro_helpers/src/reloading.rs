use ::derive_syn_parse::Parse;
use ::syn::{Token, Ident, Attribute, token::{Brace, Paren}, punctuated::Punctuated, Type};
use ::proc_macro2::TokenStream;

// Arguments passed to the reloadable attribute.
struct Arguments {

}

// We declare the names of module stuff as one big magic variable declaration.
// The special guard method receiver, the guard type, the guard lifetime,
// and the module.
struct GuardDeclaration {

}

// We declare the functions exposed by the module.
struct FnDeclaration {
    fn_token: Token![fn],
}

pub fn reloadable(_: TokenStream, _: TokenStream) -> TokenStream {
    TokenStream::new()
}
